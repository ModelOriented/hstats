#' Univariate Grid
#' 
#' @description
#' Finds evaluation grid for any numeric or non-numeric vector `z`. 
#' 
#' For discrete `z` (non-numeric, or numeric with at most `grid_size` unique values), 
#' this is simply `sort(unique(z))`.
#' 
#' Otherwise, if `strategy = "quantile"` (default), the evaluation points are computed
#' as quantiles over a regular grid of probabilities from `trim[1]` to `trim[2]`. 
#' If `strategy = "uniform"`, the evaluation points are the result of [pretty()] over
#' the trimmed range of `z`. Set `trim = c(0, 1)` for no trimming.
#' 
#' Quantiles are calculated based on the inverse of the ECDF, i.e., with
#' `stats::quantile(..., type = 1`).
#' 
#' @param z A vector/factor.
#' @param grid_size Approximate grid size.
#' @param trim A non-discrete numeric `z` is trimmed at these quantile probabilities
#'   before calculations. Set to `c(0, 1)` for no trimming.
#' @param strategy How to find evaluation points of non-discrete numeric columns? 
#'   Either "quantile" or "uniform" (via [pretty()]), see description of 
#'   [univariate_grid()].
#' @returns A vector/factor of evaluation points.
#' @examples
#' univariate_grid(iris$Species)
#' univariate_grid(rev(iris$Species))                       # Same
#' 
#' x <- iris$Sepal.Width
#' univariate_grid(x, grid_size = 5)                        # Quantile binning
#' univariate_grid(x, grid_size = 3, strategy = "uniform")  # Uniform pretty
univariate_grid <- function(z, grid_size = 36L, trim = c(0.01, 0.99), 
                            strategy = c("quantile", "uniform")) {
  strategy <- match.arg(strategy)
  uni <- unique(z)
  if (!is.numeric(z) || length(uni) <= grid_size) {
    return(sort(uni))
  }
  
  # Non-discrete numeric
  if (strategy == "quantile") {
    p <- seq(trim[1L], trim[2L], length.out = grid_size)
    g <- stats::quantile(z, probs = p, names = FALSE, type = 1L, na.rm = TRUE)
    return(unique(g))
  }
  
  # strategy = "uniform" (should use range() if trim = c(0, 1)?)
  r <- stats::quantile(z, probs = trim, names = FALSE, type = 1L, na.rm = TRUE)
  pretty(r, n = grid_size)
}

#' Multivariate Grid
#'
#' This function creates a multivariate grid. Each column of the input `x` is turned 
#' (independently) into a vector of grid values via [univariate_grid()]. 
#' Combinations are then formed by calling [expand.grid()].
#'
#' @inheritParams univariate_grid
#' @param x A vector, matrix, or data.frame to turn into a grid of values.
#' @param grid_size Controls the approximate grid size. If `x` has p columns, then each
#'   (non-discrete) column will be reduced to about the p-th root of `grid_size` values.
#' @returns A vector, matrix, or data.frame with evaluation points.
#' @examples
#' multivariate_grid(iris[1:2], grid_size = 4)
#' multivariate_grid(iris$Species)  # Works also in the univariate case
multivariate_grid <- function(x, grid_size = 36L, trim = c(0.01, 0.99),
                              strategy = c("quantile", "uniform")) {
  strategy <- match.arg(strategy)
  p <- NCOL(x)
  if (p == 1L) {
    if (is.data.frame(x)) {
      x <- x[[1L]]
    }
    return(univariate_grid(x, grid_size = grid_size, trim = trim, strategy = strategy))
  }
  grid_size <- ceiling(grid_size^(1/p))  # take p's root of grid_size
  is_mat <- is.matrix(x)
  if (is_mat) {
    x <- as.data.frame(x)
  }
  out <- expand.grid(
    lapply(
      x, 
      FUN = univariate_grid, grid_size = grid_size, trim = trim, strategy = strategy
    )
  )
  if (is_mat) as.matrix(out) else out
}

#' Checks Consistency of Grid
#' 
#' Internal function used to check if a grid of values is consistent with `v` and data.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param g Grid of values (either a vector/factor, a matrix, or data.frame).
#' @param v Vector of variable names to be represented by the grid `g`.
#' @param X_is_matrix Logical flag indicating whether the background data is a matrix.
#'   or a data.frame. `g` must be consistent with this.
#' @returns An error message or `TRUE`.
check_grid <- function(g, v, X_is_matrix) {
  p <- length(v)
  if (p != NCOL(g)) {
    stop("NCOL(grid) must equal length(v)")
  }
  if (p == 1L) {
    if (!is.vector(g) && !is.factor(g)) {
      stop("'grid' should be a vector/factor")
    }
  } else {
    stopifnot(
      is.matrix(g) || is.data.frame(g),
      is.matrix(g) == X_is_matrix,
      !is.null(colnames(g)),
      all(v == colnames(g))
    )
  }
  TRUE
}

#' Checks/Converts Predictions
#' 
#' Checks if predictions are numeric. Furthermore converts non-vector, non-matrices
#' to a matrix.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Object representing model predictions.
#' @returns Like `x`, but converted to matrix.
check_pred <- function(x) {
  if (!is.vector(x) && !is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) {
    stop("Predictions must be numeric")
  }
  x
}

#' Set Column Names
#' 
#' Internal function. If `x` does not have column names, they are set to "y" 
#' (case p = 1) or "y1", "y2", ...
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A matrix or `data.frame`.
#' @returns `x` with column names.
fix_names <- function(x) {
  if (is.null(colnames(x))) {
    p <- ncol(x)
    colnames(x) <- if (p == 1L) "y" else paste0("y", seq_len(p))
  }
  x
}

#' Fast Weighted Mean Grouped by Fixed Groups
#' 
#' Internal workhorse to aggregate predictions per evaluation point of a PD.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Vector or matrix.
#' @param ngroups Number of groups of fixed length `NROW(x) %/% ngroups`.
#' @param w Optional vector with case weights.
#' @returns A (g x K) matrix, where g is the grid size, and K = NCOL(x).
rowmean <- function(x, ngroups, w = NULL) {
  p <- NCOL(x)
  n_bg <- NROW(x) %/% ngroups
  g <- rep(seq_len(ngroups), each = n_bg)
  # Even faster: cbind(collapse::fmean(x, g = g, w = w))
  if (is.null(w)) {
    out <- rowsum(x, group = g, reorder = FALSE) / n_bg
  } else {
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
#' @param x A matrix.
#' @param w Optional case weights.
#' @returns A vector of column means.
wcolMeans <- function(x, w = NULL) {
  if (!is.matrix(x)) {
    stop("x must be a matrix")
  }
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w) 
}

#' Mean Centering of Columns
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
.center <- function(x, w = NULL) {
  if (is.vector(x)) {
    x <- matrix(x)
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
