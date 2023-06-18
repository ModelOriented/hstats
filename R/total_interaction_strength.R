#' Total Interaction Strength
#' 
#' 
#' @inheritParams H2_overall
#' @inherit total_interaction_strength references
#' @returns Vector of total interaction strength (one value per prediction dimension).
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interaction_statistics(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' total_interaction_strength(inter)
#' 
#' \dontrun{
#' total_interaction_strength(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interaction_statistics(fit, v = v, X = iris, verbose = FALSE)
#' total_interaction_strength(inter)
#' total_interaction_strength(fit, v = v, X = iris, verbose = FALSE)
#' 
#' # MODEL THREE: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' inter <- interaction_statistics(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' total_interaction_strength(inter)
#' total_interaction_strength(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' }
total_interaction_strength <- function(object, ...) {
  UseMethod("total_interaction_strength")
}

#' @describeIn total_interaction_strength Default method of total interaction strength.
#' @export
total_interaction_strength.default <- function(object, v, X, pred_fun = stats::predict,
                                               n_max = 300L, w = NULL, 
                                               verbose = TRUE, normalize = TRUE, 
                                               squared = TRUE, eps = 1e-8, ...) {
  
  istat <- interaction_statistics(
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
  total_interaction_strength(
    istat, normalize = normalize, squared = squared, sort = FALSE, eps = eps
  )
}

#' @describeIn total_interaction_strength Total interaction strength from "ranger" models.
#' @export
total_interaction_strength.ranger <- function(object, v, X, 
                                              pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                              n_max = 300L, w = NULL, verbose = TRUE,
                                              normalize = TRUE, squared = TRUE, 
                                              eps = 1e-8, ...) {
  total_interaction_strength.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = 0L,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize, 
    squared = squared, 
    eps = eps,
    ...
  )
}

#' @describeIn total_interaction_strength Total interaction strength from "mlr3" models.
#' @export
total_interaction_strength.Learner <- function(object, v, X, 
                                               pred_fun = function(m, X) m$predict_newdata(X)$response,
                                               n_max = 300L, w = NULL, verbose = TRUE,
                                               normalize = TRUE, squared = TRUE,
                                               eps = 1e-8, ...) {
  total_interaction_strength.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = 0,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize, 
    squared = squared, 
    eps = eps,
    ...
  )
}

#' @describeIn total_interaction_strength Total interaction strength from 
#'   "interaction_statistics".
#' @export
total_interaction_strength.interaction_statistics <- function(object, normalize = TRUE, 
                                                              squared = TRUE, 
                                                              eps = 1e-8, ...) {
  postprocess(
    num = with(object, wcolMeans((f - Reduce("+", F_j))^2, w = w)),
    denom = object[["mean_f2"]],
    normalize = normalize, 
    squared = squared, 
    sort = FALSE, 
    eps = eps
  )
}
