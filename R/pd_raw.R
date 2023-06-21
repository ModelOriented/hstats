#' Barebone Partial Dependence (PD) Function
#' 
#' Workhorse of the package.
#' 
#' @inheritParams pd
#' @param compress_X If `X` has a single non-`v` column: should duplicates be removed
#'   and compensated via case weights? Default is `TRUE`.
#' @param compress_grid Should duplicates in `grid` be removed and PDs mapped back to 
#'   the original grid index? Default is `TRUE`.
#' @returns 
#'   A matrix of partial dependence values (one column per prediction dimension, 
#'   one row per grid row, in the same order as `grid`).
#' @inherit pd references
pd_raw <- function(object, v, X, grid, pred_fun = stats::predict,
                   w = NULL, compress_X = TRUE, compress_grid = TRUE, ...) {
  # Try different compressions
  p <- length(v)
  D1 <- p == 1L
  if (compress_X && p >= ncol(X) - 1L) {
    # Removes duplicates in X[, not_v] and compensates via w
    cmp_X <- .compress_X(X = X, v = v, w = w)
    X <- cmp_X[["X"]]
    w <- cmp_X[["w"]]
  }
  
  if (compress_grid) {
    # Removes duplicates in grid and returns reindex vector to match to original grid
    cmp_grid <- .compress_grid(grid = grid, v = v)
    grid <- cmp_grid[["grid"]]
  }
  
  # Now, the real work starts
  n <- nrow(X)
  n_grid <- NROW(grid)
  
  # Explode everything to n * n_grid rows
  X_pred <- X[rep(seq_len(n), times = n_grid), , drop = FALSE]
  if (D1) {
    grid_pred <- rep(grid, each = n)
  } else {
    grid_pred <- grid[rep(seq_len(n_grid), each = n), ]
  }
  
  # Vary v
  if (D1 && is.data.frame(X_pred)) {
    X_pred[[v]] <- grid_pred  #  [, v] <- slower if df
  } else {
    X_pred[, v] <- grid_pred
  }
  
  # Calculate predictions and aggregate results
  pred <- align_pred(pred_fun(object, X_pred, ...))
  pd <- rowmean(pred, ngroups = n_grid, w = w)
  
  # Map back to grid order
  if (compress_grid && !is.null(reindex <- cmp_grid[["reindex"]])) {
    return(pd[reindex, , drop = FALSE])
  }
  pd
}
