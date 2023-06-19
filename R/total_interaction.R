#' Total Interaction Strength
#' 
#' Calculates proportion of variability of prediction function that comes from
#' interaction effects.
#' 
#' @inheritParams pd_raw
#' @inheritParams interact
#' @inheritParams H2_overall
#' @inherit interact references
#' @returns Vector of total interaction strength (one value per prediction dimension).
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' total_interaction(inter)
#' 
#' \dontrun{
#' total_interaction(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' total_interaction(inter)
#' total_interaction(fit, v = v, X = iris, verbose = FALSE)
#' 
#' # MODEL THREE: No interactions
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' total_interaction(inter)
#' }
total_interaction <- function(object, ...) {
  UseMethod("total_interaction")
}

#' @describeIn total_interaction Default method of total interaction strength.
#' @export
total_interaction.default <- function(object, v, X, pred_fun = stats::predict,
                                      n_max = 400L, w = NULL, verbose = TRUE, 
                                      normalize = TRUE, squared = TRUE, eps = 1e-8, 
                                      ...) {
  istat <- interact(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = 0L,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
  total_interaction(
    istat, normalize = normalize, squared = squared, sort = FALSE, eps = eps
  )
}

#' @describeIn total_interaction Total interaction strength from "ranger" models.
#' @export
total_interaction.ranger <- function(object, v, X, 
                                     pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                     n_max = 400L, w = NULL, verbose = TRUE,
                                     normalize = TRUE, squared = TRUE, 
                                     eps = 1e-8, ...) {
  total_interaction.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize, 
    squared = squared, 
    eps = eps,
    ...
  )
}

#' @describeIn total_interaction Total interaction strength from "mlr3" models.
#' @export
total_interaction.Learner <- function(object, v, X, 
                                      pred_fun = function(m, X) m$predict_newdata(X)$response,
                                      n_max = 400L, w = NULL, verbose = TRUE,
                                      normalize = TRUE, squared = TRUE, 
                                      eps = 1e-8, ...) {
  total_interaction.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize, 
    squared = squared, 
    eps = eps,
    ...
  )
}

#' @describeIn total_interaction Total interaction strength from "interact" object.
#' @export
total_interaction.interact <- function(object, normalize = TRUE, 
                                       squared = TRUE, eps = 1e-8, ...) {
  postprocess(
    num = with(object, wcolMeans((f - Reduce("+", F_j))^2, w = w)),
    denom = object[["mean_f2"]],
    normalize = normalize, 
    squared = squared, 
    sort = FALSE, 
    eps = eps
  )
}
