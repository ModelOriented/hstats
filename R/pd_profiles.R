#' Fast Partial Dependence Function
#' 
#' @description
#' Fast implementation of Friedman's (empirical) partial dependence (PD) function 
#' of feature subset \eqn{J}, given by
#' \deqn{\textrm{PD}_J(v) = \frac{1}{|D|} \sum_{i \in D} \hat f(v, x_{i,\setminus J})},
#' where
#' - \eqn{D} is a reference data set,
#' - \eqn{J} is a index set
#' - \eqn{\hat f} is the fitted model, and
#' - \eqn{x_{i,\setminus J}} is the feature vector of the i-th observation without 
#'   components in \eqn{J} (which are replaced by the function argument(s) \eqn{v}).
#' 
#' The function supports both 
#' - multivariate predictions (e.g., multi-classification settings) and
#' - multivariate grids.
#' 
#' @inheritParams pd_raw
#' @inheritParams make_grid
#' @param grid Named list. Each element specifies the evaluation grid for the
#'   corresponding feature. Missing components are automatically added. If `v` has
#'   length 1, then `grid` can also be a vector.
#' @param verbose Should a progress bar be shown? The default is `TRUE`.
#' @returns 
#'   An object of class "pd_profiles", containing these elements:
#'   - `grid`: Named list of evaluation points.
#'   - `pd`: Named list of PD matrices.
#'   - `v`: Same as input `v`.
#' @references
#'   Friedman, Jerome H. "Greedy Function Approximation: A Gradient Boosting Machine." 
#'     Annals of Statistics 29 (2000): 1189-1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' pd <- pd_profiles(fit, v = names(iris[-1]), X = iris)
#' pd
#' head(summary(pd))
#' summary(pd, "Species")
#' 
#' pd <- pd_profiles(fit, v = "Petal.Width", X = iris, grid = seq(0, 1, by = 0.5))
#' summary(pd)
#' summary(pd_profiles(fit, v = "Petal.Width", X = iris, grid = seq(1, 0, by = -0.5)))
#' summary(pd_profiles(fit, v = "Species", X = iris))
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- names(iris[3:5])
#' partial_grid <- list(Petal.Width = seq(0, 1, by = 0.5))
#' pd <- pd_profiles(fit, v = v, X = iris, grid = partial_grid, verbose = FALSE)
#' summary(pd, "Species")
#' summary(pd, "Petal.Width")
#' head(summary(pd, "Petal.Length"))
#' 
#' # MODEL THREE: Gamma GLM with log link
#' fit <- glm(
#'   Sepal.Length ~ . + Petal.Width:Species, 
#'   data = iris, 
#'   family = Gamma(link = log)
#' )
#' summary(pd_profiles(fit, v = "Species", X = iris, type = "response"))
pd_profiles <- function(object, ...) {
  UseMethod("pd_profiles")
}

#' @describeIn pd_profiles Default method.
#' @export
pd_profiles.default <- function(object, v, X, pred_fun = stats::predict, 
                                grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                                strategy = c("quantile", "uniform"), n_max = 1000L, 
                                w = NULL, verbose = TRUE, ...) {
  strategy <- match.arg(strategy)
  p <- length(v)
  .basic_check(X = X, v = v, pred_fun = pred_fun, w = w)
  
  # Make list of grid values per v
  if ((p == 1L) && (is.vector(grid) || is.factor(grid))) {
    grid <- stats::setNames(list(grid), v)
  } else {
    for (z in v) {
      if (is.null(grid[[z]])) {
        zz <- if (is.data.frame(X)) X[[z]] else X[, z]
        grid[[z]] <- make_grid_one(
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
  
  # Initialize progress bar
  show_bar <- verbose && p >= 2L
  if (show_bar) {
    j <- 1L
    pb <- utils::txtProgressBar(1L, p, style = 3)
  }
  
  # Calculations
  pd <- stats::setNames(vector("list", length = p), v)
  for (z in v) {
    pd[[z]] <- pd_raw(
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
    if (show_bar) {
      utils::setTxtProgressBar(pb, j)
      j <- j + 1L
    }
  }
  if (show_bar) {
    cat("\n")
  }
  structure(list(grid = grid, pd = pd, v = v), class = "pd_profiles")
}

#' @describeIn pd_profiles Method for "ranger" models.
#' @export
pd_profiles.ranger <- function(object, v, X, 
                               pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                               grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                               strategy = c("quantile", "uniform"), n_max = 1000L,
                               w = NULL, verbose = TRUE, ...) {
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
    verbose = verbose,
    ...
  )
}

#' @describeIn pd_profiles Method for "mlr3" models.
#' @export
pd_profiles.Learner <- function(object, v, X, 
                                pred_fun = function(m, X) m$predict_newdata(X)$response, 
                                grid = NULL, grid_size = 36L, trim = c(0.01, 0.99),
                                strategy = c("quantile", "uniform"), n_max = 1000L, 
                                w = NULL, verbose = TRUE, ...) {
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
    verbose = verbose,
    ...
  )
}

#' pd_profiles Print
#' 
#' Print function for result of [pd_profiles()].
#' 
#' @param x Object to print.
#' @param ... Currently unused.
#' @returns Invisibly, `x` is returned.
#' @export
#' @seealso [pd_profiles()]
print.pd_profiles <- function(x, ...) {
  cat("PD values. Use summary(pd, variable) to extract the results.")
  invisible(x)
}

#' pd_profiles Summary
#' 
#' Uses the results of [pd_profiles()] to show PD values for a given variable.
#' 
#' @param object Object to summarize.
#' @param which Name or position (within `v`) of feature to show partial dependence.
#' @param out_names Optional names of the output columns corresponding to the 
#'   K-dimensional predictions.
#' @param ... Currently unused.
#' @returns data.frame with evaluation grid and PD values (one column per prediction).
#' @export
#' @seealso [pd_profiles()]
summary.pd_profiles <- function(object, which = 1L, out_names = NULL, ...) {
  cbind.data.frame(
    object[["grid"]][which], 
    fix_names(object[["pd"]][[which]], out_names = out_names)
  )
}

