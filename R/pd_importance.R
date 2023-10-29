#' PD Bases Importance (Experimental)
#' 
#' Experimental variable importance method based on partial dependence functions. 
#' While related to Greenwell et al., our suggestion measures not only main effect
#' strength but also interaction effects. It is very closely related to \eqn{H^2_j}, 
#' see Details. Use `plot()` to get a barplot.
#' 
#' If \eqn{x_j} has no effects, the (centered) prediction function \eqn{F}
#' equals the (centered) partial dependence \eqn{F_{\setminus j}} on all other 
#' features \eqn{\mathbf{x}_{\setminus j}}, i.e.,
#' \deqn{
#'     F(\mathbf{x}) = F_{\setminus j}(\mathbf{x}_{\setminus j}).
#' }
#' Therefore, the following measure of variable importance follows:
#' \deqn{
#'   \textrm{PDI}_j = \frac{\frac{1}{n} \sum_{i = 1}^n\big[F(\mathbf{x}_i) - 
#'   \hat F_{\setminus j}(\mathbf{x}_{i\setminus j})\big]^2}{\frac{1}{n} \sum_{i = 1}^n
#'   \big[F(\mathbf{x}_i)\big]^2}.
#' }
#' It differs from \eqn{H^2_j} only by not subtracting the main effect of the \eqn{j}-th 
#' feature in the numerator. It can be read as the proportion of prediction variability 
#' unexplained by all other features. As such, it measures variable importance of 
#' the \eqn{j}-th feature, including its interaction effects (check [partial_dep()] 
#' for all definitions).
#' 
#' Remarks 1 to 4 of [h2_overall()] also apply here.
#' 
#' @inheritParams h2_overall
#' @inherit h2_overall return
#' @seealso [hstats()], [perm_importance()]
#' @references
#'   Greenwell, Brandon M., Bradley C. Boehmke, and Andrew J. McCarthy.  
#'     *A Simple and Effective Model-Based Variable Importance Measure.* Arxiv (2018).
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ . , data = iris)
#' s <- hstats(fit, X = iris[, -1])
#' plot(pd_importance(s))
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[, 1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' s <- hstats(fit, X = iris[, 3:5])
#' plot(pd_importance(s))
pd_importance <- function(object, ...) {
  UseMethod("pd_importance")
}

#' @describeIn pd_importance Default method of PD based feature importance.
#' @export
pd_importance.default <- function(object, ...) {
  stop("No default method implemented.")
}

#' @describeIn pd_importance PD based feature importance from "hstats" object.
#' @export
pd_importance.hstats <- function(object, normalize = TRUE, squared = TRUE,
                                 sort = TRUE, zero = TRUE, ...) {
  get_hstats_matrix(
    statistic = "pd_importance",
    object = object,
    normalize = normalize, 
    squared = squared, 
    sort = sort,
    zero = zero
  )
}

#' Raw PDI
#' 
#' Internal helper function that calculates numerator and denominator of
#' statistic in title.
#' 
#' @noRd
#' @keywords internal
#' @param x A list containing the elements "v", "K", "pred_names", 
#'   "f", "F_not_j", "mean_f2", "eps", and "w".
#' @returns A list with the numerator and denominator statistics.
pd_importance_raw <- function(x) {
  num <- init_numerator(x, way = 1L)
  for (z in x[["v"]]) {
    num[z, ] <- with(x, wcolMeans((f - F_not_j[[z]])^2, w = w))
  }
  num <- .zap_small(num, eps = x[["eps"]])  # Numeric precision
  list(num = num, denom = x[["mean_f2"]])
}
