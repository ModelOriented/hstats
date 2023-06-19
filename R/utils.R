#' Aligns Predictions
#' 
#' Turns predictions into matrix with column names.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Object representing model predictions.
#' @returns Like `x`, but converted to matrix.
align_pred <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) {
    stop("Predictions must be numeric")
  }
  if (is.null(colnames(x))) {
    p <- ncol(x)
    colnames(x) <- if (p == 1L) "y" else paste0("y", seq_len(p))
  }
  x
}

#' Fast Weighted Mean by Fixed-Length Groups
#' 
#' Internal workhorse to aggregate predictions per evaluation point of a PD.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Vector or matrix.
#' @param ngroups Number of groups of fixed length `NROW(x) / ngroups`.
#' @param w Optional vector with case weights of length `NROW(x) / ngroups`.
#' @returns A (g x K) matrix, where g is the grid size, and K = NCOL(x).
rowmean <- function(x, ngroups, w = NULL) {
  p <- NCOL(x)
  n_bg <- NROW(x) %/% ngroups
  g <- rep(seq_len(ngroups), each = n_bg)
  # Even faster: cbind(collapse::fmean(x, g = g, w = w))
  if (is.null(w)) {
    out <- rowsum(x, group = g, reorder = FALSE) / n_bg
  } else {
    if (length(w) != n_bg) {
      stop("w must be of length NROW(x) / ngroups.")
    }
    # w is recycled over rows and columns
    out <- rowsum(x * w, group = g, reorder = FALSE) / sum(w)
  }
  rownames(out) <- NULL
  out
}

#' Compresses X
#' 
#' @description
#' Internal function to remove duplicated rows in `X` based on columns not in `v`. 
#' Compensation is done by summing corresponding case weights `w`. 
#' Currently implemented only for the case when there is a single non-`v` column in `X`.
#' Can later be generalized to multiple columns via [paste()]. 
#' 
#' Notes:
#' - This function is important for interaction calculations.
#' - The initial check for having a single non-`v` column is very cheap.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams pd_raw
#' @returns A list with `X` and `w`, potentially compressed.
.compress_X <- function(X, v, w = NULL) {
  not_v <- setdiff(colnames(X), v)
  if (length(not_v) != 1L) {
    return(list(X = X, w = w))  # No optimization implemented for this case
  }
  x_not_v <- if (is.data.frame(X)) X[[not_v]] else X[, not_v]
  X_dup <- duplicated(x_not_v)
  if (!any(X_dup)) {
    return(list(X = X, w = w))  # No optimization done
  }

  # Compress
  if (is.null(w)) {
    w <- rep(1.0, times = nrow(X))
  }
  list(
    X = X[!X_dup, , drop = FALSE], 
    w = c(rowsum(w, group = x_not_v, reorder = FALSE))
  )
}

#' Compresses Grid
#' 
#' Internal function used to remove duplicated grid rows. Re-indexing to original grid 
#' rows needs to be later, but this function provides the re-index vector to do so.
#' Further note that checking for uniqueness can be costly for higher-dimensional grids.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams pd_raw
#' @returns 
#'   A list with `grid` (possibly compressed) and the optional `reindex` vector
#'   used to map PD values back to the original grid rows.
.compress_grid <- function(grid, v) {
  ugrid <- unique(grid)
  if (NROW(ugrid) == NROW(grid)) {
    # No optimization done
    return(list(grid = grid))
  }
  out <- list(grid = ugrid)
  if (length(v) >= 2L) {  # Non-vector case
    grid <- apply(grid, MARGIN = 1L, FUN = paste, collapse = "_:_")
    ugrid <- apply(ugrid, MARGIN = 1L, FUN = paste, collapse = "_:_")
    if (anyDuplicated(ugrid)) {
      stop("String '_:_' found in grid values at unlucky position.")
    }
  }
  out[["reindex"]] <- match(grid, ugrid)
  out
}

#' Zip Small Values
#' 
#' Internal function. Sets very small or non-finite (NA, ...) values in vector, 
#' matrix or data.frame to 0.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Vector, matrix, or data.frame.
#' @param eps Threshold, below which absolute values are set to 0.
#' @returns Same as `x` but with values below `eps` replaced by 0.
.zap_small <- function(x, eps = 1e-8) {
  zero <- abs(x) < eps | !is.finite(x)
  if (any(zero)) {
    x[zero] <- 0
  }
  x
}

#' Weighted Version of colMeans()
#' 
#' Internal function used to calculate column-wise weighted means.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A matrix-like object.
#' @param w Optional case weights.
#' @returns A vector of column means.
wcolMeans <- function(x, w = NULL) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w) 
}

#' Weighted Mean Centering
#' 
#' Internal function used to center each column of an object by subtracting its 
#' possibly weighted mean. Vector input is turned into a matrix with one column.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Matrix, data.frame, or vector.
#' @param w Optional vector of case weights.
#' @returns Centered version of `x` (vectors are turned into single-column matrix).
wcenter <- function(x, w = NULL) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  sweep(x, MARGIN = 2L, STATS = wcolMeans(x, w = w))
}

#' Basic Checks
#' 
#' Internal function used to check some basic things.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams pd_raw
#' @returns Error or TRUE
basic_check <- function(X, v, pred_fun, w) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= c(1L, 1L),
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X)
  )
  TRUE
}

#' Raw H2 Overall
#' 
#' Function used to calculate Friedman and Popescu's overall H-squared. It is separated
#' from the main H2_overall function because it is used within interaction_statistics
#' to select the variables for pairwise calculations.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param F_j List of empirical PD values per feature.
#' @param F_not_j List of empirical PD values of all features except j.
#' @param f Predictions.
#' @param mean_f2 Weighted average of f^2.
#' @param w Optional case weights.
#' @inheritParams H2_overall
H2_overall_raw <- function(F_j, F_not_j, f, mean_f2, w = NULL, 
                           normalize = TRUE, squared = TRUE, sort = TRUE, 
                           top_m = Inf, eps = 1e-8, ...) {
  v <- names(F_j)
  num <- matrix(nrow = length(v), ncol = ncol(f), dimnames = list(v, colnames(f)))
  for (z in v) {
    num[z, ] <- wcolMeans((f - F_j[[z]] - F_not_j[[z]])^2, w = w)
  }
  postprocess(
    num = num,
    denom = mean_f2,
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}

#' Postprocessing of Statistics
#' 
#' Function to apply typical postprocessing steps to a Friedman-Popescu type statistic.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams H2_overall
#' @param num Numerator of statistic.
#' @param denom Denominator of statistic.
#' @returns Matrix of statistics.
postprocess <- function(num, denom = 1, normalize = TRUE, squared = TRUE, 
                        sort = TRUE, top_m = Inf, eps = 1e-8) {
  out <- .zap_small(num, eps = eps)
  if (normalize) {
    out <- out / denom
  }
  if (!squared) {
    out <- sqrt(out)
  }
  if (sort) {
    out <- out[order(-rowSums(out)), , drop = FALSE]
  }
  utils::head(out, n = top_m)
}

combine_by <- function(x, to_numeric) {
  x <- lapply(names(x), function(nm) data.frame(by = nm, x[[nm]]))
  if (to_numeric) {
    x[["by"]] <- as.numeric(x[["by"]])
  }
  do.call(rbind, x)
}
