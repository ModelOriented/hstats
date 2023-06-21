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
#' (check [pd_profiles()] for all definitions).
#'
#' @section Remarks:
#' 
#' 1. Remarks 1 to 4 of [H2_pairwise()] also apply here.
#' 2. \eqn{H^2_{jk} = 0} means there are no interaction effects between \eqn{x_j}
#'   and \eqn{x_k}. The larger the value, the more of the joint effect of the two 
#'   features comes from the interaction.
#' 3. Since the denominator differs between variable pairs, unlike \eqn{H_j}, 
#'   this test statistic is difficult to compare between variable pairs. 
#'   If both main effects are very weak, a negligible interaction can get a 
#'   high \eqn{H^2_{jk}}. Therefore, Friedman and Popescu (2008) suggests to calculate 
#'   \eqn{H^2_{jk}} only for *important* variables.
#'   
#' @section Alternatives:
#' 
#' To be able to compare pairwise interaction strength across variable pairs, 
#' and to overcome the problem mentioned in the last remark, we suggest as alternative 
#' a different denominator, namely the same as used for \eqn{H^2_j}:
#' \deqn{
#'   \tilde H^2_{jk} = \frac{A_{jk}}{\frac{1}{n} \sum_{i = 1}^n\big[F(\mathbf{x}_i)\big]^2}.
#' }
#' This statistic would tell us how much of the total variance of the predictions comes 
#' from the pairwise interaction of \eqn{x_j} and \eqn{x_k}. Use `denominator = "F"` to
#' apply this variant.
#' 
#' Another possibility would be to use the unnormalized test statistic on the scale 
#' of the predictions, i.e., \eqn{\sqrt{A_{jk}}}. Set `normalize = FALSE` and 
#' `squared = FALSE` to get this statistic.
#' 
#' @inheritParams H2_overall
#' @param denominator Should the denominator be the variation of the combined 
#'   effect \eqn{F_{jk}} (like Friedman and Popescu, default) or rather the
#'   variation of the predictions. The latter has the advantage that values can 
#'   directly be compared across variable pairs (and are still relative). 
#'   Only relevant when `normalize = TRUE`.
#' @returns 
#'   Matrix of interactions statistics (one row per variable pair, one column per
#'   prediction dimension).
#' @inherit interact references
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' H2_pairwise(inter)                     # Proportion of pairwise effect variability
#' H2_pairwise(inter, denominator = "F")  # Proportion of prediction variability
#' 
#' \dontrun{
#' H2_pairwise(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' H2_pairwise(inter)
#' H2_pairwise(fit, v = v, X = iris, verbose = FALSE)
#' }
H2_pairwise <- function(object, ...) {
  UseMethod("H2_pairwise")
}

#' @describeIn H2_pairwise Default pairwise interaction strength.
#' @export
H2_pairwise.default <- function(object, v, X, pred_fun = stats::predict,
                                pairwise_m = 5L, n_max = 300L, w = NULL, verbose = TRUE,
                                normalize = TRUE, denominator = c("F_jk", "F"),
                                squared = TRUE, sort = TRUE, 
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
  H2_pairwise(
    istat,
    normalize = normalize,
    denominator = denominator,
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}

#' @describeIn H2_pairwise Pairwise interaction strength from "ranger" models.
#' @export
H2_pairwise.ranger <- function(object, v, X, 
                               pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                               pairwise_m = 5L, n_max = 300L, w = NULL, verbose = TRUE,
                               normalize = TRUE, denominator = c("F_jk", "F"),
                               squared = TRUE, sort = TRUE, top_m = Inf, eps = 1e-8, 
                               ...) {
  H2_pairwise.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = pairwise_m,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize,
    denominator = denominator,
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps,
    ...
  )
}

#' @describeIn H2_pairwise Pairwise interaction strength from "mlr3" models.
#' @export
H2_pairwise.Learner <- function(object, v, X, 
                                pred_fun = function(m, X) m$predict_newdata(X)$response,
                                pairwise_m = 5L, n_max = 300L, w = NULL, verbose = TRUE,
                                normalize = TRUE, denominator = c("F_jk", "F"), 
                                squared = TRUE, sort = TRUE, top_m = Inf, eps = 1e-8, 
                                ...) {
  H2_pairwise.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = pairwise_m,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize,
    denominator = denominator,
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps,
    ...
  )
}

#' @describeIn H2_pairwise Pairwise interaction strength from "interact" object.
#' @export
H2_pairwise.interact <- function(object, normalize = TRUE, 
                                 denominator = c("F_jk", "F"),
                                 squared = TRUE, sort = TRUE, 
                                 top_m = Inf, eps = 1e-8, ...) {
  denominator <- match.arg(denominator)
  combs <- object[["combs"]]
  n_combs <- length(combs)
  nms <- colnames(object[["F"]])
  num <- denom <- matrix(
    nrow = n_combs, ncol = length(nms), dimnames = list(names(combs), nms)
  )
  for (i in seq_len(n_combs)) {
    z <- combs[[i]]
    num[i, ] <- with(
      object, wcolMeans((F_jk[[i]] - F_j[[z[1L]]] - F_j[[z[2L]]])^2, w = w)
    )
    denom[i, ] <- with(
      object, 
      if (denominator == "F_jk") wcolMeans(F_jk[[i]]^2, w = w) else mean_f2
    )
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
