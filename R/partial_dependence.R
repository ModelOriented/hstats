#' Partial Dependence Profiles
#' 
#' Estimates the partial dependence function of features `v` over a 
#' grid of values.
#' 
#' @section Partial Dependence Functions: 
#' 
#' Let \eqn{F: R^p \to R} denote the prediction function that maps the 
#' \eqn{p}-dimensional feature vector \eqn{\mathbf{x} = (x_1, \dots, x_p)}
#' to its prediction. Furthermore, let 
#' \deqn{
#'   F_s(\mathbf{x}_s) = E_{\mathbf{x}_{\setminus s}}(F(\mathbf{x}_s, \mathbf{x}_{\setminus s}))
#' }
#' be the partial dependence function of \eqn{F} on the feature subset
#' \eqn{\mathbf{x}_s}, where \eqn{s \subseteq \{1, \dots, p\}}, as introduced in 
#' Friedman (2001). Here, the expectation runs over the joint marginal distribution
#' of features \eqn{\mathbf{x}_{\setminus s}} not in \eqn{\mathbf{x}_s}.
#' 
#' Given data, \eqn{F_s(\mathbf{x}_s)} can be estimated by the empirical partial 
#' dependence function
#' 
#' \deqn{
#'   \hat F_s(\mathbf{x}_s) = \frac{1}{n} \sum_{i = 1}^n F(\mathbf{x}_s, \mathbf{x}_{i\setminus s}),
#' }
#' where \eqn{\mathbf{x}_{i\setminus s}} \eqn{i = 1, \dots, n}, are the observed values
#' of \eqn{\mathbf{x}_{\setminus s}}.
#' 
#' @inheritParams multivariate_grid
#' @param object Fitted model object.
#' @param v Vector of feature names.
#' @param X A data.frame or matrix serving as background dataset.
#' @param grid A vector (if `length(v) == 1L`), or a matrix/data.frame otherwise.
#'   If `NULL`, calculated via [multivariate_grid()].
#' @param pred_fun Prediction function of the form `function(object, X, ...)`,
#'   providing K >= 1 numeric predictions per row. Its first argument represents the 
#'   model `object`, its second argument a data structure like `X`. Additional arguments 
#'   (such as `type = "response"` in a GLM) can be passed via `...`. The default, 
#'   [stats::predict()], will work in most cases. Note that column names in a resulting
#'   matrix of predictions will be used as default column names in the results.
#' @param n_max If `X` has more than `n_max` rows, a random sample of `n_max` rows is
#'   selected from `X`. In this case, set a random seed for reproducibility.
#' @param w Optional vector of case weights for each row of `X`.
#' @param grid Named list. Each element specifies the evaluation grid for the
#'   corresponding feature. Missing components are automatically generated via
#'   [univariate_grid()]. If `v` has length 1, then `grid` can also be a vector.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`, for instance
#'   `type = "response"` in a [glm()] model.
#' @returns 
#'   A list of PD profiles per variable in `v`. Has additional class "partial_dependence".
#' @references
#'   Friedman, Jerome H. *"Greedy Function Approximation: A Gradient Boosting Machine."* 
#'     Annals of Statistics 29, no. 5 (2001): 1189-1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' pd <- partial_dependence(fit, v = "Species", X = iris)
#' pd$Species
#' head(pd$Sepal.Width)
#' 
#' pd <- partial_dependence(fit, v = "Petal.Width", X = iris, grid = seq(1, 0, by = -0.5))
#' pd$Petal.Width 
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- names(iris[3:5])
#' partial_grid <- list(Petal.Width = seq(0, 1, by = 0.5))
#' pd <- partial_dependence(fit, v = v, X = iris, grid = partial_grid, verbose = FALSE)
#' pd$Species
#'  
#' # MODEL THREE: Gamma GLM -> pass options to predict() via ...
#' fit <- glm(
#'   Sepal.Length ~ . + Petal.Width:Species, 
#'   data = iris, 
#'   family = Gamma(link = log)
#' )
#' partial_dependence(fit, v = "Species", X = iris, type = "response")$Species
partial_dependence <- function(object, ...) {
  UseMethod("partial_dependence")
}

#' @describeIn partial_dependence Default method.
#' @export
partial_dependence.default <- function(object, v, X, pred_fun = stats::predict,
                                       grid = NULL, grid_size = 36L, 
                                       trim = c(0.01, 0.99), 
                                       strategy = c("quantile", "uniform"), 
                                       n_max = 1000L, w = NULL, ...) {
  basic_check(X = X, v = v, pred_fun = pred_fun, w = w)
  
  if (is.null(grid)) {
    grid <- multivariate_grid(
      x = X[, v], grid_size = grid_size, trim = trim, strategy = strategy
    )
  } else {
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

  # Calculations
  pd <- pd_raw(
    object = object, 
    v = v, 
    X = X, 
    grid = grid,
    pred_fun = pred_fun,
    w = w,
    compress_grid = FALSE,  # Almost always unique, so we save a check for uniqueness
    ...
  )
  pd <- cbind.data.frame(grid, pd)
  structure(list(pd = pd, v = v), class = "partial_dependence")
}

#' @describeIn partial_dependence Method for "ranger" models.
#' @export
partial_dependence.ranger <- function(object, v, X, 
                                      pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                                      grid = NULL, grid_size = 36L, 
                                      trim = c(0.01, 0.99), 
                                      strategy = c("quantile", "uniform"), 
                                      n_max = 1000L, w = NULL, ...) {
  partial_dependence.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    n_max = n_max,
    w = w,
    ...
  )
}

#' @describeIn partial_dependence Method for "mlr3" models.
#' @export
partial_dependence.Learner <- function(object, v, X, 
                                       pred_fun = function(m, X) m$predict_newdata(X)$response, 
                                       grid = NULL, grid_size = 36L, 
                                       trim = c(0.01, 0.99),
                                       strategy = c("quantile", "uniform"), 
                                       n_max = 1000L, w = NULL, ...) {
  partial_dependence.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    n_max = n_max,
    w = w,
    ...
  )
}
