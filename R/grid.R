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
#' @export
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
#' @export
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
