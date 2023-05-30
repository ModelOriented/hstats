#' Fast Partial Dependence Function
#' 
#' @description
#' Fast implementation of Friedman's partial dependence (PD) function. 
#' 
#' The function supports both 
#' - multivariate predictions (e.g., multi-classification settings) and
#' - multivariate grids.
#' 
#' @param object Fitted model object.
#' @param v One or more variable names in `X` to calculate partial dependence profiles.
#' @param X Dataframe or matrix serving as background dataset.
#' @param pred_fun Prediction function of the form `function(object, X, ...)`,
#'   providing \eqn{K \ge 1} numeric predictions per row. Its first argument 
#'   represents the model `object`, its second argument a data structure like `X`. 
#'   Additional (named) arguments are passed via `...`. 
#'   The default, [stats::predict()], will work in most cases. 
#' @param grid Optional evaluation grid. If `v` is a single column name, this is a 
#'   vector/factor. Otherwise, a matrix or `data.frame` with `length(v)` columns.
#' @param grid_size Determines the grid When `grid = NULL`. Character/factor variables
#'   are evaluated at each unique value. A numeric `v` with more than `grid_size` unique
#'   values is evaluated at `grid_size` quantiles. If `v` has length \eqn{p > 1},
#'   the \eqn{p}th root of `grid_size` is used instead. 
#' @param trim A vector with two probabilities used to trim non-discrete numeric `v` 
#'   before applying quantile binning (only if `grid = NULL`). 
#'   Set to `c(0, 1)` to avoid trimming.
#' @param n_max If `X` has more rows than `n_max`, a random sample of `n_max` rows is
#'   selected. 
#' @param out_names Names of the output columns.
#' @param w Optional vector of case weights for each row of `X`.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`.
#' @returns A dataframe with partial dependence per grid value.
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'     The Annals of Statistics, 29:1189–1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' pd <- fx_pdp(fit, v = "Petal.Width", X = iris)
#' pd[1:4, ]
#' 
#' fx_pdp(fit, v = "Petal.Width", X = iris, grid = seq(0, 1, by = 0.5), out_name = "P")
#' fx_pdp(fit, v = "Petal.Width", X = iris, grid = seq(1, 0, by = -0.5))
#' fx_pdp(fit, v = "Species", X = iris)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' fx_pdp(fit, v = "Species", X = iris)
#' pd <- fx_pdp(fit, v = c("Petal.Width", "Species"), X = iris)
#' pd[1:4, ]
#' 
#' # MODEL THREE: Gamma GLM with log link
#' fit <- glm(Sepal.Length ~ Species + Petal.Width, data = iris, family = Gamma(link = log))
#' fx_pdp(fit, v = "Petal.Width", X = iris, type = "response")

fx_pdp <- function(object, ...) {
  UseMethod("fx_pdp")
}

#' @describeIn fx_pdp Default method.
#' @export
fx_pdp.default <- function(object, v, X, pred_fun = stats::predict, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           n_max = 1000L, out_names = NULL, w = NULL, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= 2:1,
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X)
  )
  
  # Reduce size of X (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
  }

  # Make/check grid. If length(v) == 1, grid is always a vector/factor
  if (is.null(grid)) {
    grid <- fixed_grid(X[, v], m = grid_size, trim = trim)
    compress_grid <- FALSE
  } else {
    check_grid(grid, v = v, X_is_matrix = is.matrix(X))
    compress_grid <- TRUE
  }
  
  # Calculations
  pd <- pdp_raw(
    object = object, 
    v = v, 
    X = X, 
    pred_fun = pred_fun, 
    grid = grid, 
    w = w,
    compress_grid = compress_grid,
    ...
  )
  
  # Cleanup
  pd <- fix_names(pd, out_names = out_names)
  if (!is.data.frame(grid)) {
    grid <- stats::setNames(as.data.frame(grid), v)
  }
  cbind.data.frame(grid, pd)
}


#' @describeIn fx_pdp Method for "ranger" models.
#' @export
fx_pdp.ranger <- function(object, v, X, 
                          pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                          grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                          n_max = 1000L, out_names = NULL, w = NULL, ...) {
  fx_pdp.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    n_max = n_max,
    out_names = out_names,
    w = w,
    ...
  )
}

#' @describeIn fx_pdp Method for "mlr3" models.
#' @export
fx_pdp.Learner <- function(object, v, X, 
                           pred_fun = function(m, X) m$predict_newdata(X)$response, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           n_max = 1000L, out_names = NULL, w = NULL, ...) {
  fx_pdp.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    n_max = n_max,
    out_names = out_names,
    w = w,
    ...
  )
}

#' Barebone PDP
#' 
#' @description
#' Creates partial dependence values for a given model, data and grid. 
#' This is the workhorse function behind [fx_pdp()] and [fx_friedmans_h()].
#' 
#' It is fast because:
#' 1. Only one call to `pred_fun()`.
#' 2. If more than 10% of grid values are duplicated, these are dropped and the result
#'   mapped back to the original grid.
#' 3. If more than 10% of non-grid columns are duplicated, these are dropped and
#'   compensated by summing up the case weights `w`.
#' 
#' @noRd
#' 
#' @inheritParams fx_pdp
#' @param grid A vector (if `length(v) == 1L`), or a matrix/data.frame otherwise.
#' @returns 
#'   A matrix of partial dependence values. The number of columns corresponds to the
#'   number of predictions per observation.
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'   The Annals of Statistics, 29:1189–1232.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' pdp_raw(fit, v = "Petal.Width", X = iris, pred_fun = predict, grid = 1:2)
pdp_raw <- function(object, v, X, pred_fun, grid, w = NULL, 
                    compress_X = FALSE, compress_grid = TRUE, ...) {
  if (compress_X) {
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
  
  pred <- check_pred(pred_fun(object, X_pred, ...))
  pd <- rowmean(pred, ngroups = n_grid, w = w)
  if (compress_grid && !is.null(reindex <- cmp_grid[["reindex"]])) {
    return(pd[reindex, , drop = FALSE])
  }
  pd
}

