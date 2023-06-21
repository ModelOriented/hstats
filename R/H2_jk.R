#' Pairwise Interaction Strength
#' 
#' Friedman and Popescu's statistics of pairwise interaction strength, 
#' ideally calculated from the result of [interact()].
#' 
#' @details
#' Following Friedman and Popescu (2008), if there are no interaction effects between 
#' features \eqn{x_j} and \eqn{x_k}, their two-dimensional (centered) partial dependence 
#' function \eqn{F_{jk}} can be written as the sum of the (centered) univariate partial 
#' dependencies \eqn{F_j} and \eqn{F_k}, i.e.,
#' \deqn{
#'   F_{jk}(x_j, x_k) = F_j(x_j)+ F_k(x_k).
#' }
#' Correspondingly, Friedman and Popescu's \eqn{H_{jk}^2} statistic of pairwise interaction 
#' strength is defined as
#' \deqn{
#'   H_{jk}^2 = \frac{A_{jk}}{B_{jk}},
#' }
#' where
#' \deqn{
#'    A_{jk} = \frac{1}{n} \sum_{i = 1}^n\big[\hat F_{jk}(x_{ij}, x_{ik}) - \hat F_j(x_{ij}) - \hat F_k(x_{ik})\big]^2
#' }
#' and
#' \deqn{
#'   B_{jk} = \frac{1}{n} \sum_{i = 1}^n\big[\hat F_{jk}(x_{ij}, x_{ik})\big]^2
#' }
#' (check [pd()] for all definitions).
#'
#' @section Remarks:
#' 
#' 1. Remarks 1 to 4 of [H2_j()] also apply here.
#' 2. \eqn{H^2_{jk} = 0} means there are no interaction effects between \eqn{x_j}
#'   and \eqn{x_k}. The larger the value, the more of the joint effect of the two 
#'   features comes from the interaction.
#' 3. Since the denominator differs between variable pairs, unlike \eqn{H_j}, 
#'   this test statistic is difficult to compare between variable pairs. 
#'   If both main effects are very weak, a negligible interaction can get a 
#'   high \eqn{H^2_{jk}}. Therefore, Friedman and Popescu (2008) suggests to calculate 
#'   \eqn{H^2_{jk}} only for *important* variables.
#'   
#' @section Modification:
#' 
#' To be better able to compare pairwise interaction strength across variable pairs, 
#' and to overcome the problem mentioned in the last remark, we suggest as alternative 
#' the unnormalized test statistic on the scale of the predictions, 
#' i.e., \eqn{\sqrt{A_{jk}}}. Set `normalize = FALSE` and `squared = FALSE` for this.
#' Furthermore, we calculate pairwise statistics only for features with highest
#' \eqn{H^2_{j}}.
#' 
#' @inheritParams interact
#' @inheritParams H2_j
#' @returns 
#'   Matrix of interactions statistics (one row per variable pair, one column per
#'   prediction dimension).
#' @inherit interact references
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # Proportion of joint effect coming from pairwise interaction
#' H2_jk(inter)
#' 
#' # Absolute measure
#' H2_jk(inter, normalize = FALSE, squared = FALSE)  
#' 
#' \dontrun{
#' H2_jk(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' H2_jk(inter)
#' H2_jk(fit, v = v, X = iris, verbose = FALSE)
#' }
H2_jk <- function(object, ...) {
  UseMethod("H2_jk")
}

#' @describeIn H2_jk Default pairwise interaction strength.
#' @export
H2_jk.default <- function(object, v, X, pred_fun = stats::predict,
                          pairwise_m = 5L, n_max = 300L, w = NULL, verbose = TRUE,
                          normalize = TRUE, squared = TRUE, sort = TRUE, 
                          top_m = Inf, eps = 1e-8, ...) {
  istat <- interact(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = pairwise_m,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
  H2_jk(
    istat,
    normalize = normalize,
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}

#' @describeIn H2_jk Pairwise interaction strength from "ranger" models.
#' @export
H2_jk.ranger <- function(object, v, X, 
                         pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                         pairwise_m = 5L, n_max = 300L, w = NULL, verbose = TRUE,
                         normalize = TRUE, squared = TRUE, sort = TRUE, 
                         top_m = Inf, eps = 1e-8, ...) {
  H2_jk.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = pairwise_m,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize,
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps,
    ...
  )
}

#' @describeIn H2_jk Pairwise interaction strength from "mlr3" models.
#' @export
H2_jk.Learner <- function(object, v, X, 
                          pred_fun = function(m, X) m$predict_newdata(X)$response,
                          pairwise_m = 5L, n_max = 300L, w = NULL, verbose = TRUE,
                          normalize = TRUE, squared = TRUE, sort = TRUE,
                          top_m = Inf, eps = 1e-8, ...) {
  H2_jk.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = pairwise_m,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize,
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps,
    ...
  )
}

#' @describeIn H2_jk Pairwise interaction strength from "interact" object.
#' @export
H2_jk.interact <- function(object, normalize = TRUE, squared = TRUE, sort = TRUE, 
                           top_m = Inf, eps = 1e-8, ...) {
  combs <- object[["combs"]]
  n_combs <- length(combs)
  nms <- colnames(object[["f"]])
  num <- denom <- matrix(
    nrow = n_combs, ncol = length(nms), dimnames = list(names(combs), nms)
  )
  for (i in seq_len(n_combs)) {
    z <- combs[[i]]
    num[i, ] <- with(
      object, wcolMeans((F_jk[[i]] - F_j[[z[1L]]] - F_j[[z[2L]]])^2, w = w)
    )
    denom[i, ] <- if (normalize) with(object, wcolMeans(F_jk[[i]]^2, w = w)) else 1
  }
  postprocess(
    num = num,
    denom = denom,
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}
