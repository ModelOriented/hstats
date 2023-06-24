#' Total Interaction Strength
#' 
#' Proportion of prediction variability unexplained by main effects (extracted from the
#' result of [interact()]), see Details. The statistic requires `v` to represent all
#' features of the model. Otherwise, it will also contain effects from features missing
#' in `v`.
#' 
#' @details
#' If the model is additive in all features, then the (centered) prediction 
#' function \eqn{F} equals the sum of the (centered) partial dependence 
#' functions \eqn{F_j(x_j)}, i.e.,
#' \deqn{
#'   F(\mathbf{x}) = \sum_{j}^{p} F_j(x_j)
#' }
#' (check [partial_dep()] for all definitions).
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
#' @inheritParams H2_j
#' @returns Vector of total interaction strength (one value per prediction dimension).
#' @export
#' @seealso [interact()], [H2_j()], [H2_jk()]
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' H2(inter)
#' 
#' \dontrun{
#' H2(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' H2(inter)
#' 
#' # MODEL THREE: No interactions
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' H2(inter)
#' }
H2 <- function(object, ...) {
  UseMethod("H2")
}

#' @describeIn H2 Default method of total interaction strength.
#' @export
H2.default <- function(object, ...) {
  stop("No default method implemented.")
}

#' @describeIn H2 Total interaction strength from "interact" object.
#' @export
H2.interact <- function(object, normalize = TRUE, 
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
