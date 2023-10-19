#' Aligns Predictions
#' 
#' Turns predictions into matrix.
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
#' @param ngroups Number of groups (`x` was stacked that many times).
#' @param w Optional vector with case weights of length `NROW(x) / ngroups`.
#' @returns A (g x K) matrix, where g is the number of groups, and K = NCOL(x).
wrowmean <- function(x, ngroups = 1L, w = NULL) {
  if (ngroups == 1L) {
    return(rbind(wcolMeans(x, w = w)))
  }
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
    w = c(rowsum(w, group = x_not_v, reorder = FALSE))  # warning if missing in x_not_v
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
#'   used to map compressed grid values back to the original grid rows. The original
#'   grid equals the compressed grid at indices `reindex`.
.compress_grid <- function(grid) {
  ugrid <- unique(grid)
  if (NROW(ugrid) == NROW(grid)) {
    # No optimization done
    return(list(grid = grid, reindex = NULL))
  }
  out <- list(grid = ugrid)
  if (NCOL(grid) >= 2L) {  # Non-vector case
    grid <- apply(grid, MARGIN = 1L, FUN = paste, collapse = "_:_")
    ugrid <- apply(ugrid, MARGIN = 1L, FUN = paste, collapse = "_:_")
    if (anyDuplicated(ugrid)) {
      stop("String '_:_' found in grid values at unlucky position.")
    }
  }
  out[["reindex"]] <- match(grid, ugrid)
  out
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
  if (NCOL(x) == 1L && is.null(w)) {
    return(mean(x))
  }
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w) 
}

#' Grouped wcolMeans()
#' 
#' Internal function used to calculate grouped column-wise weighted means.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A matrix-like object.
#' @param g Optional grouping variable.
#' @param w Optional case weights.
#' @param reorder Should groups be ordered, see [rowsum()]. Default is `TRUE`. 
#' @returns A matrix with one row per group.
gwColMeans <- function(x, g = NULL, w = NULL, reorder = TRUE) {
  if (is.null(g)) {
    return(rbind(wcolMeans(x, w = w)))
  }
  if (is.null(w)) {
    num <- rowsum(x, group = g, reorder = reorder)
    denom <- rowsum(rep.int(1, NROW(x)), group = g, reorder = reorder)
  } else {
    num <- rowsum(x * w, group = g, reorder = reorder)
    denom <- rowsum(w, group = g, reorder = reorder)
  }
  num / matrix(denom, nrow = nrow(num), ncol = ncol(num), byrow = FALSE)
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
  # sweep(x, MARGIN = 2L, STATS = wcolMeans(x, w = w))  # Slower
  x - matrix(wcolMeans(x, w = w), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
}
