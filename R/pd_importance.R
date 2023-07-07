#' PD Bases Importance (Experimental)
#' 
#' Experimental variable importance method based on partial dependence functions. 
#' While related to Greenwell et al., our suggestion measures not only main effect
#' strength but also interaction effects. It is very closely related to the
#' \eqn{H^2_j} statistics, see Details. By default, the results are plotted as barplot.
#' Set `plot = FALSE` to get numbers.
#' 
#' @details
#' If \eqn{x_j} has no effects, the (centered) prediction function \eqn{F}
#' equals the (centered) partial dependence \eqn{F_{\setminus j}} on all other 
#' features \eqn{\mathbf{x}_{\setminus j}}, i.e.,
#' \deqn{
#'     F(\mathbf{x}) = F_{\setminus j}(\mathbf{x}_{\setminus j}).
#' }
#' Therefore, the following measure of variable importance follows:
#' \deqn{
#'   \textrm{Imp}_{j} = \frac{\frac{1}{n} \sum_{i = 1}^n\big[F(\mathbf{x}_i) - 
#'   \hat F_{\setminus j}(\mathbf{x}_{i\setminus j})\big]^2}{\frac{1}{n} \sum_{i = 1}^n
#'   \big[F(\mathbf{x}_i)\big]^2}.
#' }
#' It differs from \eqn{H^2_j} only by not subtracting the main effect of the \eqn{j}-th 
#' feature in the numerator. It can be read as the proportion of prediction variability 
#' unexplained by all other features. As such, it measures variable importance of 
#' the \eqn{j}-th feature, including its interaction effects (check [partial_dep()] 
#' for all definitions).
#' 
#' Remarks 1 to 4 of [H2_overall()] also apply here.
#' 
#' @inheritParams H2_overall
#' @inherit H2_overall return
#' @seealso [interact()], [H2_overall()]
#' @references
#'   Greenwell, Brandon M., Bradley C. Boehmke, and Andrew J. McCarthy.  
#'     *A Simple and Effective Model-Based Variable Importance Measure.* Arxiv (2018).
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' pd_importance(inter)
#' pd_importance(inter, plot = FALSE)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' pd_importance(inter)
pd_importance <- function(object, ...) {
  UseMethod("pd_importance")
}

#' @describeIn pd_importance Default method of PD based feature importance.
#' @export
pd_importance.default <- function(object, ...) {
  stop("No default method implemented.")
}

#' @describeIn pd_importance PD based feature importance from "interact" object.
#' @export
pd_importance.interact <- function(object, normalize = TRUE, squared = TRUE, 
                                   sort = TRUE, top_m = 15L, eps = 1e-8, plot = TRUE, 
                                   fill = "#2b51a1", ...) {
  num <- with(
    object, matrix(nrow = length(v), ncol = K, dimnames = list(v, pred_names))
  )
  for (z in object[["v"]]) {
    num[z, ] <- with(object, wcolMeans((f - F_not_j[[z]])^2, w = w))
  }
  out <- postprocess(
    num = num,
    denom = object[["mean_f2"]],
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
  if (plot) plot_stat(out, fill = fill, ...) else out
}
