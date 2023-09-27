#' Total Interaction Strength
#' 
#' Proportion of prediction variability unexplained by main effects of `v`, see Details. 
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
#' @inheritParams h2_overall
#' @param ... Currently unused.
#' @returns Vector of total interaction strength (one value per prediction dimension).
#' @export
#' @seealso [hstats()], [h2_overall()], [h2_pairwise()], [h2_threeway()]
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
#' s <- hstats(fit, X = iris[-1])
#' h2(s)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' s <- hstats(fit, X = iris[3:5])
#' h2(s)
#' 
#' # MODEL 3: No interactions
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' s <- hstats(fit, X = iris[-1], verbose = FALSE)
#' h2(s)
h2 <- function(object, ...) {
  UseMethod("h2")
}

#' @describeIn h2 Default method of total interaction strength.
#' @export
h2.default <- function(object, ...) {
  stop("No default method implemented.")
}

#' @describeIn h2 Total interaction strength from "interact" object.
#' @export
h2.hstats <- function(object, normalize = TRUE, squared = TRUE, eps = 1e-8, ...) {
  postprocess(
    num = object$h2$num,
    denom = object$h2$denom,
    normalize = normalize, 
    squared = squared,
    sort = FALSE,
    eps = eps
  )
}

#' Raw H2
#' 
#' Internal helper function that calculates numerator and denominator of
#' statistic in title.
#' 
#' @noRd
#' @keywords internal
#' @param x A list containing the elements "f", "F_j", "w", and "mean_f2".
#' @returns A list with the numerator and denominator statistics.
h2_raw <- function(x) {
  list(
    num = with(x, wcolMeans((f - Reduce("+", F_j))^2, w = w)), 
    denom = x[["mean_f2"]]
  )
}

