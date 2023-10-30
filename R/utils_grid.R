#' Univariate Grid
#' 
#' @description
#' Creates evaluation grid for any numeric or non-numeric vector `z`. 
#' 
#' For discrete `z` (non-numeric, or numeric with at most `grid_size` unique values), 
#' this is simply `sort(unique(z))`.
#' 
#' Otherwise, if `strategy = "uniform"` (default), the evaluation points form a regular
#' grid over the trimmed range of `z`. By trimmed range we mean the
#' range of `z` after removing values outside `trim[1]` and `trim[2]` quantiles.
#' Set `trim = 0:1` for no trimming.
#' 
#' If `strategy = "quantile"`, the evaluation points are quantiles over a regular grid 
#' of probabilities from `trim[1]` to `trim[2]`.
#' 
#' Quantiles are calculated via the inverse of the ECDF, i.e., via
#' `stats::quantile(..., type = 1`).
#' 
#' @param z A vector or factor.
#' @param grid_size Approximate grid size.
#' @param trim The default `c(0.01, 0.99)` means that values outside the 
#'   1% and 99% quantiles of non-discrete numeric columns are removed before calculation 
#'   of grid values. Set to `0:1` for no trimming.
#' @param strategy How to find grid values of non-discrete numeric columns? 
#'   Either "uniform" or "quantile", see description of [univariate_grid()].
#' @param na.rm Should missing values be dropped from the grid? Default is `TRUE`.
#' @returns A vector or factor of evaluation points.
#' @seealso [multivariate_grid()]
#' @export
#' @examples
#' univariate_grid(iris$Species)
#' univariate_grid(rev(iris$Species))                       # Same
#' 
#' x <- iris$Sepal.Width
#' univariate_grid(x, grid_size = 5)                        # Uniform binning
#' univariate_grid(x, grid_size = 5, strategy = "quantile")  # Quantile
univariate_grid <- function(z, grid_size = 49L, trim = c(0.01, 0.99), 
                            strategy = c("uniform", "quantile"), na.rm = TRUE) {
  strategy <- match.arg(strategy)
  uni <- unique(z)
  if (!is.numeric(z) || length(uni) <= grid_size) {
    out <- if (na.rm) sort(uni) else sort(uni, na.last = TRUE)
    return(out)
  }
  
  # Non-discrete numeric
  if (strategy == "quantile") {
    p <- seq(trim[1L], trim[2L], length.out = grid_size)
    g <- stats::quantile(z, probs = p, names = FALSE, type = 1L, na.rm = TRUE)
    out <- unique(g)
  } else {
    # strategy = "uniform"
    if (trim[1L] == 0 && trim[2L] == 1) {
      r <- range(z, na.rm = TRUE)
    } else {
      r <- stats::quantile(z, probs = trim, names = FALSE, type = 1L, na.rm = TRUE)
    }
    out <- seq(r[1L], r[2L], length.out = grid_size)  
  }
  if (!na.rm && anyNA(z)) {
    out <- c(out, NA)
  }
  return(out)
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
#' @seealso [univariate_grid()]
#' @examples
#' multivariate_grid(iris[1:2], grid_size = 4)
#' multivariate_grid(iris$Species)  # Works also in the univariate case
#' @export
multivariate_grid <- function(x, grid_size = 49L, trim = c(0.01, 0.99),
                              strategy = c("uniform", "quantile"), na.rm = TRUE) {
  strategy <- match.arg(strategy)
  p <- NCOL(x)
  if (p == 1L) {
    if (is.data.frame(x)) {
      x <- x[[1L]]
    }
    out <- univariate_grid(
      x, grid_size = grid_size, trim = trim, strategy = strategy, na.rm = na.rm
    )
    return(out)
  }
  grid_size <- ceiling(grid_size^(1/p))  # take p's root of grid_size
  is_mat <- is.matrix(x)
  if (is_mat) {
    x <- as.data.frame(x)
  }
  out <- expand.grid(
    lapply(
      x, 
      FUN = univariate_grid, 
      grid_size = grid_size, 
      trim = trim, 
      strategy = strategy,
      na.rm = na.rm
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
    if (is.list(g)) {
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
