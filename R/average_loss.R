#' Average Loss
#'
#' Calculates average loss, optionally grouped by a discrete vector. 
#' The function supports multivariate losses and case weights.
#'
#' @inheritParams hstats
#' @param y Numeric vector or matrix of the response corresponding to `X`.
#' @param loss One of "squared_error", "logloss", "mlogloss", "poisson",
#'   "gamma", "absolute_error", or a loss function that turns observed and predicted 
#'   values (vectors or matrices) into a vector or matrix of unit losses.
#'   For "mlogloss", the response `y` can either be a matrix with one column per category
#'   or a vector with categories. The latter case is internally exploded to the shape
#'   of the predictions via `stats::model.matrix(~ y + 0)`.
#' @param BY Optional grouping vector.
#' @returns A matrix with one row per group and one column per loss dimension.
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' average_loss(fit, X = iris, y = iris$Sepal.Length)
#' average_loss(fit, X = iris, y = iris$Sepal.Length, BY = iris$Species)
#'
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' average_loss(fit, X = iris, y = iris[1:2])
#' average_loss(fit, X = iris, y = iris[1:2], loss = "gamma", BY = iris$Species)
average_loss <- function(object, ...) {
  UseMethod("average_loss")
}

#' @describeIn average_loss Default method.
#' @export
average_loss.default <- function(object, X, y, 
                                 pred_fun = stats::predict,
                                 BY = NULL, loss = "squared_error", w = NULL, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    nrow(X) >= 1L,
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X),
    NROW(y) == nrow(X)
  )
  if (!is.null(BY)) {
    stopifnot(
      NCOL(BY) == 1L,
      is.vector(BY) || is.factor(BY),
      length(BY) == nrow(X)
    )
  }
  if (!is.function(loss) && loss == "mlogloss" && NCOL(y) == 1L) {
    y <- stats::model.matrix(~y + 0)
  }
  if (!is.matrix(y)) {
    y <- as.matrix(y)
  }
  if (!is.function(loss)) {
    loss <- get_loss_fun(loss)
  }
  
  L <- loss(y, align_pred(pred_fun(object, X, ...)))
  gwColMeans(L, g = BY, w = w)
}

#' @describeIn average_loss Method for "ranger" models.
#' @export
average_loss.ranger <- function(object, X, y, 
                                pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                BY = NULL, loss = "squared_error", w = NULL, ...) {
  average_loss.default(
    object = object, 
    X = X, 
    y = y, 
    pred_fun = pred_fun, 
    BY = BY, 
    loss = loss, 
    w = w, 
    ...
  )
}

#' @describeIn average_loss Method for "mlr3" models.
#' @export
average_loss.Learner <- function(object, v, X, y, 
                                 pred_fun = NULL,
                                 BY = NULL, loss = "squared_error", w = NULL, ...) {
  if (is.null(pred_fun)) {
    pred_fun <- mlr3_pred_fun(object, X = X)
  }
  average_loss.default(
    object = object, 
    X = X, 
    y = y, 
    pred_fun = pred_fun, 
    BY = BY, 
    loss = loss, 
    w = w, 
    ...
  )
}

#' @describeIn average_loss Method for DALEX "explainer".
#' @export
average_loss.explainer <- function(object, 
                                   X = object[["data"]], 
                                   y = object[["y"]], 
                                   pred_fun = object[["predict_function"]],
                                   BY = NULL, 
                                   loss = "squared_error", 
                                   w = object[["weights"]], 
                                   ...) {
  average_loss.default(
    object = object[["model"]],
    X = X,
    y = y,
    pred_fun = pred_fun,
    BY = BY,
    loss = loss,
    w = w,
    ...
  )
}
