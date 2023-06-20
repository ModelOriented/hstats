#' Total Interaction Strength
#' 
#' Calculates the proportion of variability of prediction function that comes from
#' interaction effects of all features combined.
#' 
#' @details
#' The idea is as follows: if the model is additive in all features,
#' then the (centered) prediction function \eqn{F} equals the sum of the (centered)
#' partial dependence functions \eqn{F_j(x_j)}, i.e.,
#' \deqn{
#'   F(\mathbf{x}) = \sum_{j}^{p} F_j(x_j)
#' }
#' (see [pd_profiles()] for the definitions).
#' To measure the relative amount of variability explained by all interactions, 
#' we can therefore study the test statistic of total interaction strength
#' \deqn{
#'   H^2 = \frac{\frac{1}{n} \sum_{i = 1}^n \big[F(\mathbf{x}_i) - 
#'   \sum_{j = 1}^p\hat F_j(x_{ij})\big]^2}{\frac{1}{n} 
#'   \sum_{i = 1}^n\big[F(\mathbf{x}_i)\big]^2}.
#' }
#' It equals the variability of the predictions unexplained by the main effects. 
#' A value of 0 would mean there are no interaction effects at all.
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
                                      n_max = 300L, w = NULL, verbose = TRUE, 
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
                                     n_max = 300L, w = NULL, verbose = TRUE,
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
                                      n_max = 300L, w = NULL, verbose = TRUE,
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
