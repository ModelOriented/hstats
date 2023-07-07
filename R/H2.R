#' Total Interaction Strength
#' 
#' Proportion of prediction variability unexplained by main effects of `v` 
#' (extracted from the result of [interact()]), see Details. 
#' 
#' @details
#' If the model is additive in all features, then the (centered) prediction 
#' function \eqn{F} equals the sum of the (centered) partial dependence 
#' functions \eqn{F_j(x_j)}, i.e.,
#' \deqn{
#'   F(\mathbf{x}) = \sum_{j}^{p} F_j(x_j)
#' }
#' (check [partial_dep()] for all definitions).
#' To measure the relative amount of variability unexplained by all main effects, 
#' we can therefore study the test statistic of total interaction strength
#' \deqn{
#'   H^2 = \frac{\frac{1}{n} \sum_{i = 1}^n \big[F(\mathbf{x}_i) - 
#'   \sum_{j = 1}^p\hat F_j(x_{ij})\big]^2}{\frac{1}{n} 
#'   \sum_{i = 1}^n\big[F(\mathbf{x}_i)\big]^2}.
#' }
#' A value of 0 means there are no interaction effects at all. 
#' Due to (typically undesired) extrapolation effects, depending on the model, 
#' values above 1 may occur.
#' 
#' In Żółkowski et al. (2023), \eqn{1 - H^2} is called *additivity index*. 
#' A similar measure using accumulated local effects is discussed in Molnar (2020).
#' 
#' @inheritParams H2_overall
#' @param ... Currently unused.
#' @returns Vector of total interaction strength (one value per prediction dimension).
#' @export
#' @seealso [interact()], [H2_overall()], [H2_pairwise()], [H2_threeway()]
#' @references 
#' 1. Żółkowski, Artur, Mateusz Krzyziński, and Paweł Fijałkowski. 
#'   *Methods for extraction of interactions from predictive models.* 
#'   Undergraduate thesis. Faculty of Mathematics and Information Science, 
#'   Warsaw University of Technology (2023).
#' 2. Molnar, Christoph, Giuseppe Casalicchio, and Bernd Bischl". 
#'   *Quantifying Model Complexity via Functional Decomposition for Better Post-hoc Interpretability*, 
#'   in Machine Learning and Knowledge Discovery in Databases, 
#'   Springer International Publishing (2020): 193-204.
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' H2(inter)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' H2(inter)
#' 
#' # MODEL 3: No interactions
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' H2(inter)
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
H2.interact <- function(object, normalize = TRUE, squared = TRUE, eps = 1e-8, ...) {
  postprocess(
    num = with(object, wcolMeans((f - Reduce("+", F_j))^2, w = w)),
    denom = object[["mean_f2"]],
    normalize = normalize, 
    squared = squared, 
    sort = FALSE, 
    eps = eps
  )
}
