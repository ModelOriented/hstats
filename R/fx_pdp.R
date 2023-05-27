#' Fast PDP
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
#'   before applying quantile binning. Set to `c(0, 1)` to avoid trimming.
#'   Not used with `grid = NULL`. 
#' @param n_max If `X` has more rows than `n_max`, a random sample of `n_max` rows is
#'   selected. 
#' @param w Optional vector of case weights for each row of `X`.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`.
#' @returns A dataframe with partial dependence per grid value.
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'   The Annals of Statistics, 29:1189–1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
#' pd <- fx_pdp(fit, v = "Petal.Width", X = iris)
#' pd[1:4, ]
#' 
#' fx_pdp(fit, v = "Petal.Width", X = iris, grid = seq(0, 1, by = 0.5), pd_name = "P")
#' fx_pdp(fit, v = "Petal.Width", X = iris, grid = seq(1, 0, by = -0.5))
#' fx_pdp(fit, v = "Species", X = iris)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' fx_pdp(fit, v = "Species", X = iris)
#' pd <- fx_pdp(fit, v = c("Petal.Width", "Species"), X = iris)
#' pd[1:4, ]
fx_pdp <- function(object, ...) {
  UseMethod("fx_pdp")
}

#' @describeIn fx_pdp Default method.
#' @export
fx_pdp.default <- function(object, v, X, pred_fun = stats::predict, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           n_max = 500L, pd_names = NULL, w = NULL, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= 2:1,
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X)
  )
  
  # Reduce size of X (and w)
  n <- nrow(X)
  if (n > n_max) {
    ix <- sample(n, n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
    n <- n_max
  }

  # Make/check grid. If length(v) == 1, grid is always a vector/factor
  if (is.null(grid)) {
    grid <- fixed_grid(X[, v], m = grid_size, trim = trim)
  } else{
    check_grid(grid, v = v, X_is_matrix = is.matrix(X))
  }
  
  # Calculations
  out <- pdp_raw(
    object = object, v = v, X = X, pred_fun = pred_fun, grid = grid, w = NULL, ...
  )
  cbind.data.frame(out[["grid"]], fix_pd_names(out[["pd"]], pd_names = pd_names))
}


#' @describeIn fx_pdp Method for "ranger" models, see Readme for an example.
#' @export
fx_pdp.ranger <- function(object, v, X, 
                          pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                          grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                          n_max = 500L, pd_names = NULL, w = NULL, ...) {
  fx_pdp.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    n_max = n_max,
    pd_names = pd_names,
    w = w,
    ...
  )
}

#' @describeIn fx_pdp Method for "mlr3" models, see Readme for an example.
#' @export
fx_pdp.Learner <- function(object, v, X, 
                           pred_fun = function(m, X) m$predict_newdata(X)$response, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           n_max = 500L, pd_names = NULL, w = NULL, ...) {
  fx_pdp.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    n_max = n_max,
    pd_names = pd_names,
    w = w,
    ...
  )
}

# Barebone function. Arguments see fx_pdp()
# If length(v) == 1, then grid must be a vector/factor.
# Returns named matrix of "pd", and evaluation "grid" (always a df)
pdp_raw <- function(object, v, X, pred_fun, grid, w = NULL, ...) {
  n <- nrow(X)
  n_grid <- NROW(grid)
  D1 <- length(v) == 1L
  
  # Explode everything to n * n_grid rows
  X_pred <- X[rep(seq_len(n), times = n_grid), , drop = FALSE]
  if (D1) {
    grid_pred <- rep(grid, each = n)
  } else {
    grid_pred <- grid[rep(seq_len(n_grid), each = n), ]
  }
  if (!is.null(w)) {
    w <- rep(w, times = n_grid)
  }
  
  # Vary v
  if (D1 && is.data.frame(X_pred)) {
    X_pred[[v]] <- grid_pred  #  [, v] <- much slower if df
  } else {
    X_pred[, v] <- grid_pred
  }
  
  # Create **matrix** of predictions
  pred <- pred2matrix(pred_fun(object, X_pred, ...))
  
  # Turn grid into data.frame to simplify working with collapse
  if (!is.data.frame(grid_pred)) {
    grid_pred <- stats::setNames(as.data.frame(grid_pred), v)
  }
  g <- collapse::GRP(grid_pred, sort = FALSE)
  
  # Calculate PD (always a matrix)
  pd <- collapse::fmean(pred, g = g, w = w)
  rownames(pd) <- NULL
  list(pd = pd, grid = g[["groups"]])
}
