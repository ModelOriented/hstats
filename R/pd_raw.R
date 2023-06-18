#' Barebone Partial Dependence (PD) Function
#' 
#' Workhorse of [pd_profiles()] and [interact()]. 
#' Furthermore, it can be used to calculate multi-dimensional partial dependencies.
#' In this case, provide your own evaluation grid, for instance via [multivariate_grid()]. 
#' 
#' @param object Fitted model object.
#' @param v Vector of feature names.
#' @param X A data.frame or matrix serving as background dataset.
#' @param grid A vector (if `length(v) == 1L`), or a matrix/data.frame otherwise.
#' @param pred_fun Prediction function of the form `function(object, X, ...)`,
#'   providing K >= 1 numeric predictions per row. Its first argument represents the 
#'   model `object`, its second argument a data structure like `X`. Additional arguments 
#'   (such as `type = "response"` in a GLM) can be passed via `...`. The default, 
#'   [stats::predict()], will work in most cases. Note that column names in a resulting
#'   matrix of predictions will be used as default column names in the results.
#' @param n_max If `X` has more than `n_max` rows, a random sample of `n_max` rows is
#'   selected from `X`. In this case, set a random seed for reproducibility.
#' @param w Optional vector of case weights for each row of `X`.
#' @param compress_X If `X` has a single non-`v` column: should duplicates be removed
#'   and compensated via case weights? Default is `TRUE`.
#' @param compress_grid Should duplicates in `grid` be removed and PDs mapped back to 
#'   the original grid index? Default is `TRUE`.
#' @param check Should input checks be applied? Default is `TRUE`.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`, for instance
#'   `type = "response"` in a [glm()] model.
#' @returns 
#'   A matrix of partial dependence values (one column per prediction dimension, 
#'   one row per grid row, in the same order as `grid`).
#' @references
#'   Friedman, Jerome H. *"Greedy Function Approximation: A Gradient Boosting Machine."* 
#'     Annals of Statistics 29, no. 5 (2001): 1189-1232.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' pd_raw(fit, v = "Petal.Width", X = iris, pred_fun = predict, grid = 1:2)
#' 
#' # 2D PD
#' grid <- multivariate_grid(iris[4:5])
#' grid
#' values <- pd_raw(fit, X = iris, v = colnames(iris)[4:5], grid = grid)
#' result <- data.frame(grid, values)
#' head(result, 2)
pd_raw <- function(object, v, X, grid, pred_fun = stats::predict, n_max = 1000L, 
                   w = NULL, compress_X = TRUE, compress_grid = TRUE, check = TRUE, 
                   ...) {
  if (check) {
    basic_check(X = X, v = v, pred_fun = pred_fun, w = w)
    check_grid(g = grid, v = v, X_is_matrix = is.matrix(X))
  }
  
  # Reduce size of X (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
  }
  
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
