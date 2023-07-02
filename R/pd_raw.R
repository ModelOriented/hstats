#' Barebone Partial Dependence Function
#' 
#' Workhorse of the package, thus optimized for speed.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams partial_dep
#' @param grid A vector, data.frame or matrix of grid values consistent with `v` and `X`.
#' @param compress_X If `X` has a single non-`v` column: should duplicates be removed
#'   and compensated via case weights? Default is `TRUE`.
#' @param compress_grid Should duplicates in `grid` be removed and PDs mapped back to 
#'   the original grid index? Default is `TRUE`.
#' @returns 
#'   A matrix of partial dependence values (one column per prediction dimension, 
#'   one row per grid row, in the same order as `grid`).
pd_raw <- function(object, v, X, grid, pred_fun = stats::predict,
                   w = NULL, compress_X = TRUE, compress_grid = TRUE, ...) {
  # Try different compressions
  if (compress_X && length(v) == ncol(X) - 1L) {
    # Removes duplicates in X[, not_v] and compensates via w
    cmp_X <- .compress_X(X = X, v = v, w = w)
    X <- cmp_X[["X"]]
    w <- cmp_X[["w"]]
  }
  if (compress_grid) {
    # Removes duplicates in grid and returns reindex vector to match to original grid
    cmp_grid <- .compress_grid(grid = grid)
    grid <- cmp_grid[["grid"]]
  }
  
  # Now, the real work
  pred <- ice_raw(
    object, v = v, X = X, grid = grid, pred_fun = pred_fun, pred_only = TRUE, ...
  )
  pd <- wrowmean(pred, ngroups = NROW(grid), w = w)
  
  # Map back to grid order
  if (compress_grid && !is.null(reindex <- cmp_grid[["reindex"]])) {
    return(pd[reindex, , drop = FALSE])
  }
  pd
}

#' Barebone ICE Function
#' 
#' Part of the workhorse function `pd_raw()`, thus optimized for speed.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams pd_raw
#' @param pred_only Logical flag determining the output mode. If `TRUE`, then a matrix
#'   of predictions. Otherwise, a list with two elements: `pred` (prediction matrix)
#'   and `grid_pred` (the corresponding grid values in the same mode as the input, 
#'   but replicated over `X`).
#' @returns 
#'   Either a matrix of predictions or a list with predictions and grid.
ice_raw <- function(object, v, X, grid, pred_fun = stats::predict, 
                    pred_only = TRUE, ...) {
  D1 <- length(v) == 1L
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
  
  # Calculate matrix of predictions
  pred <- align_pred(pred_fun(object, X_pred, ...))
  
  if (pred_only) {
    return(pred)
  }
  return(list(pred = pred, grid_pred = grid_pred))
}
