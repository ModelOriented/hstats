#' Partial Dependence Profiles
#' 
#' For a set `v` of features, their individual partial dependence is evaluated over
#' evaluation grids. The grids can be prespecified for any subset of the features. 
#' The function calls the prediction function m times, where m is the number of
#' features. Multivariate predictions are supported. 
#' 
#' @inheritParams pd_raw
#' @inheritParams univariate_grid
#' @param grid Named list. Each element specifies the evaluation grid for the
#'   corresponding feature. Missing components are automatically generated via
#'   [univariate_grid()]. If `v` has length 1, then `grid` can also be a vector.
#' @returns 
#'   A list of PD profiles per variable in `v`. Has additional class "pd_profiles".
#' @references
#'   Friedman, Jerome H. "Greedy Function Approximation: A Gradient Boosting Machine." 
#'     Annals of Statistics 29 (2000): 1189-1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' pd <- pd_profiles(fit, v = names(iris[-1]), X = iris)
#' pd$Species
#' head(pd$Sepal.Width)
#' 
#' pd <- pd_profiles(fit, v = "Petal.Width", X = iris, grid = seq(1, 0, by = -0.5))
#' pd$Petal.Width 
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- names(iris[3:5])
#' partial_grid <- list(Petal.Width = seq(0, 1, by = 0.5))
#' pd <- pd_profiles(fit, v = v, X = iris, grid = partial_grid, verbose = FALSE)
#' pd$Species
#'  
#' # MODEL THREE: Gamma GLM -> pass options to predict() via ...
#' fit <- glm(
#'   Sepal.Length ~ . + Petal.Width:Species, 
#'   data = iris, 
#'   family = Gamma(link = log)
#' )
#' pd_profiles(fit, v = "Species", X = iris, type = "response")$Species
pd_profiles <- function(object, ...) {
  UseMethod("pd_profiles")
}

#' @describeIn pd_profiles Default method.
#' @export
pd_profiles.default <- function(object, v, X, pred_fun = stats::predict, 
                                grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                                strategy = c("quantile", "uniform"), n_max = 1000L, 
                                w = NULL, ...) {
  strategy <- match.arg(strategy)
  p <- length(v)
  .basic_check(X = X, v = v, pred_fun = pred_fun, w = w)
  
  # Make list of grid values per v before subsetting
  if ((p == 1L) && (is.vector(grid) || is.factor(grid))) {
    grid <- stats::setNames(list(grid), v)
  } else {
    for (z in v) {
      if (is.null(grid[[z]])) {
        zz <- if (is.data.frame(X)) X[[z]] else X[, z]
        grid[[z]] <- univariate_grid(
          zz, grid_size = grid_size, trim = trim, strategy = strategy
        )
      }
    }
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
  pd <- stats::setNames(vector("list", length = p), v)
  for (z in v) {
    temp <- pd_raw(
      object = object, 
      v = z, 
      X = X, 
      grid = grid[[z]],
      pred_fun = pred_fun,
      n_max = n_max, # No effect
      w = w,
      compress_grid = FALSE,  # Almost always unique, so we save a check for uniqueness
      check = FALSE, # Already done
      ...
    )
    pd[[z]] <- cbind.data.frame(grid[z], fix_names(temp))
  }
  structure(pd, class = "pd_profiles")
}

#' @describeIn pd_profiles Method for "ranger" models.
#' @export
pd_profiles.ranger <- function(object, v, X, 
                               pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                               grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                               strategy = c("quantile", "uniform"), n_max = 1000L,
                               w = NULL, ...) {
  pd_profiles.default(
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

#' @describeIn pd_profiles Method for "mlr3" models.
#' @export
pd_profiles.Learner <- function(object, v, X, 
                                pred_fun = function(m, X) m$predict_newdata(X)$response, 
                                grid = NULL, grid_size = 36L, trim = c(0.01, 0.99),
                                strategy = c("quantile", "uniform"), n_max = 1000L, 
                                w = NULL, ...) {
  pd_profiles.default(
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
