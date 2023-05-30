# Helper functions

#' Discretizes vector
#' 
#' Function to discretize any vector `z`. For discrete `z` (non-numeric, or numeric
#' with at most `m` unique values), this is simply `sort(unique(z))`. Otherwise, 
#' the values of `z` are first trimmed. Then, `m` quantiles are calculated using the
#' inverse of the ECDF.
#' 
#' @noRd
#' 
#' @param z A vector to discretize.
#' @param m Ideal grid size.
#' @param trim If z is non-discrete, i.e., has more than `m` unique values, `z` is
#'   trimmed first at these quantiles.
#' @returns 
#'   For discrete `z`, the result of `sort(unique(z))`. Otherwise, quantiles
#'   of trimmed values of `z`.
#' @examples
#' fixed_grid_one(iris$Species)
#' fixed_grid_one(rev(iris$Species))  # Same
#' fixed_grid_one(iris$Sepal.Width, m = 2)
fixed_grid_one <- function(z, m = 36L, trim = c(0.01, 0.99)) {
  uni <- unique(z)
  if (!is.numeric(z) || length(uni) <= m) {
    return(sort(uni))
  }
  
  # Non-discrete
  p <- seq(trim[1L], trim[2L], length.out = m)
  unique(stats::quantile(z, probs = p, names = FALSE, type = 1L))
}

#' Creates one- or higher-dimensional grids
#' 
#' Calls [fixed_grid_one()] per column and then applies [expand.grid()].
#' 
#' @noRd
#' 
#' @inheritParams fixed_grid_one
#' @param vv A vector, matrix, or data.frame to turn into a grid of values.
#' @param m Ideal grid size. If `vv` has more than one column, the corresponding
#'   root is taken from `m` to get the necessary number of grid points per dimension.
#' @returns 
#'   A vector, matrix, or data.frame with evaluation points.
#' @examples
#' fixed_grid(iris$Species)
#' fixed_grid(iris[1:2], m = 4)
fixed_grid <- function(vv, m = 36L, trim = c(0.01, 0.99)) {
  p <- NCOL(vv)
  if (p == 1L) {
    if (is.data.frame(vv)) {
      vv <- vv[[1L]]
    }
    return(fixed_grid_one(vv, m = m, trim = trim))
  }
  m <- ceiling(m^(1/p))  # take p's root of m
  is_mat <- is.matrix(vv)
  if (is_mat) {
    vv <- as.data.frame(vv)
  }
  out <- expand.grid(lapply(vv, FUN = fixed_grid_one, m = m, trim = trim))
  if (is_mat) as.matrix(out) else out
}

#' Checks Consistency of Grid
#' 
#' Checks if a grid of values is consistent with `v`.
#' 
#' @noRd
#' 
#' @param g Grid of values (either a vector/factor, or a matrix or data.frame).
#' @param v Vector of variable names to be represented by the grid `g`.
#' @param X_is_matrix Logical flag indicating whether the background data is a matrix.
#'   or a data.frame. `g` must be consistent with this.
#' @returns 
#'   An error message or `TRUE`.
#' @examples
#' fixed_grid(iris$Species)
check_grid <- function(g, v, X_is_matrix) {
  p <- length(v)
  if (p != NCOL(g)) {
    stop("NCOL(grid) must equal length(v)")
  }
  if (p == 1L) {
    if (!is.vector(g) && !is.factor(g)) {
      stop("'grid' should be a vector of values")
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
#' Checks if predictions are numeric, and are either a vector or can be converted
#' to a matrix.
#' 
#' @noRd
#' 
#' @param x Object holding predictions.
#' @returns A numeric vector or matrix with predictions.
#' @examples
#' check_pred(1:10)
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
#' Workhorse to aggregate predictions per evaluation point in a PDP.
#' 
#' @noRd
#' 
#' @param out A matrix or `data.frame`.
#' @param out_names Optional vector of column names.
#' @param prefix If `out` has no column names or if `out_names = NULL`, the column names
#'   are set to this value. If there is more than one column, the names are of the form
#'   prefix_1, prefix_2, ...
#' @returns Version of `out` with column names.
#' @examples
#' fix_names(cbind(1:3))
#' fix_names(cbind(1:2, 3:4))
#' fix_names(head(iris))
#' fix_names(head(iris), out_names = paste("V", 1:5))
fix_names <- function(out, out_names = NULL, prefix = "pred") {
  if (!is.null(out_names)) {
    colnames(out) <- out_names
  } else if (is.null(colnames(out))) {
    p <- ncol(out)
    colnames(out) <- if (p == 1L) prefix else paste(prefix, seq_len(p), sep = "_")
  }
  out
}

#' Fast Weighted Mean Grouped by Fixed Groups
#' 
#' Workhorse to aggregate predictions per evaluation point in a PDP.
#' 
#' @noRd
#' 
#' @param x Vector or matrix.
#' @param ngroups Number of groups of fixed length NROW(x) %/% ngroups.
#' @param w Optional vector with case weights.
#' @returns A (gxK) matrix, where g is the grid size, and K = NCOL(x).
#' @examples
#' rowmean(rep(2:1, each = 10), ngroups = 2)
rowmean <- function(x, ngroups, w = NULL) {
  p <- NCOL(x)
  n_bg <- NROW(x) %/% ngroups
  g <- rep(seq_len(ngroups), each = n_bg)
  # Faster: cbind(collapse::fmean(x, g = g, w = w))
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
#' Removes duplicated rows in x based on columns not in `v` and compensates by summing
#' their case weights `w`. Currently implemented only for the case where there is
#' a single non-`v` column in `X`. Can later be generalized via [paste()]. Note that
#' this is an important speed-up for calculating Friedman's H of overall interaction.
#' 
#' @noRd
#' 
#' @inheritParams fx_pdp
#' @returns A list with `X` and `w`, potentially compressed.
#' @examples
#' .compress_X(cbind(a = c(1, 1, 2), b = 1:3), v = "b")
#' .compress_X(cbind(a = c(1, 1, 2), b = 1:3), v = "b", w = 1:3)
#' .compress_X(head(iris), v = "Species")  # no effect yet
.compress_X <- function(X, v, w = NULL) {
  not_v <- setdiff(colnames(X), v)
  if (length(not_v) == 1L) {
    x_not_v <- if (is.data.frame(X)) X[[not_v]] else X[, not_v]
    X_dup <- duplicated(x_not_v)
    if (mean(X_dup) > 0.1) {
      if (is.null(w)) {
        w <- rep(1.0, times = nrow(X))
      }
      X <- X[!X_dup, , drop = FALSE]
      w <- c(rowsum(w, g = x_not_v, reorder = FALSE))
    }
  }
  return(list(X = X, w = w))
}

#' Compresses grid
#' 
#' Removes duplicated X columns (except those in `v`) and compensates by summing up
#' their case weights `w`. Currently implemented only for the case where there is
#' a single non-`v` column in `X`. Can be generalized via [paste()].
#' Note that this kicks in only if more than 5% of rows in `grid` are duplicated.
#' 
#' @noRd
#' 
#' @inheritParams pdp_raw
#' @returns 
#'   A list with `grid` (possibly compressed) and the optional `reindex` vector
#'   used to map PD values back to the original grid rows.
#' @examples
#' out <- .compress_grid(c(2, 1, 2, 2), v = "a")
#' out$grid[out$reindex]  # equals grid
#' 
#' out <- .compress_grid(cbind(a = c(2, 1, 2, 2), b = c(1, 2, 3, 3)), v = c("a", "b"))
#' out$grid[out$reindex, ]  # equals grid
.compress_grid <- function(grid, v) {
  ugrid <- unique(grid)
  if (NROW(ugrid) >= 0.95 * NROW(grid)) {
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
#' Sets very small or non-finite (NA, ...) values in vector, matrix or data.frame to 0.
#' 
#' @noRd
#' 
#' @param x Vector, matrix, or data.frame.
#' @param eps Threshold below which absolute values are set to 0.
#' @returns x with values below `eps` be replaced by 0.
#' @examples
#' .zap_small(1:4)
#' .zap_small(cbind(c(0, 1e-10, 1)))
.zap_small <- function(x, eps = 1e-8) {
  zero <- abs(x) < eps | !is.finite(x)
  if (any(zero)) {
    x[zero] <- 0
  }
  x
}

#' Mean Centering of Columns
#' 
#' Centers each column of an object by subtracting its mean. If `x` is a vector,
#' it is mean-centered as well and returned as matrix with a single column.
#' 
#' @noRd
#' 
#' @param x Matrix, data.frame, or vector.
#' @returns Centered version of `x` (vectors are turned into single-column matrix).
#' @examples
#' .center(1:10)
#' .center(iris[1:3])
.center <- function(x) {
  if (is.vector(x)) {
    x <- matrix(x)
  }
  sweep(x, MARGIN = 2L, STATS = colMeans(x))
}

