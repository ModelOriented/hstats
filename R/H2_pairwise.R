#' Pairwise Interaction Strength
#' 
#' Friedman and Popescu's statistic of pairwise interaction strength, see Details. 
#' Use `plot()` to get a barplot.
#' 
#' @details
#' Following Friedman and Popescu (2008), if there are no interaction effects between 
#' features \eqn{x_j} and \eqn{x_k}, their two-dimensional (centered) partial dependence 
#' function \eqn{F_{jk}} can be written as the sum of the (centered) univariate partial 
#' dependencies \eqn{F_j} and \eqn{F_k}, i.e.,
#' \deqn{
#'   F_{jk}(x_j, x_k) = F_j(x_j)+ F_k(x_k).
#' }
#' Correspondingly, Friedman and Popescu's statistic of pairwise 
#' interaction strength is defined as
#' \deqn{
#'   H_{jk}^2 = \frac{A_{jk}}{\frac{1}{n} \sum_{i = 1}^n\big[\hat F_{jk}(x_{ij}, x_{ik})\big]^2},
#' }
#' where
#' \deqn{
#'   A_{jk} = \frac{1}{n} \sum_{i = 1}^n\big[\hat F_{jk}(x_{ij}, x_{ik}) - 
#'    \hat F_j(x_{ij}) - \hat F_k(x_{ik})\big]^2
#' }
#' (check [partial_dep()] for all definitions).
#'
#' **Remarks:**
#' 
#' 1. Remarks 1 to 5 of [h2_overall()] also apply here.
#' 2. \eqn{H^2_{jk} = 0} means there are no interaction effects between \eqn{x_j}
#'   and \eqn{x_k}. The larger the value, the more of the joint effect of the two 
#'   features comes from the interaction.
#' 3. Since the denominator differs between variable pairs, unlike \eqn{H_j}, 
#'   this test statistic is difficult to compare between variable pairs. 
#'   If both main effects are very weak, a negligible interaction can get a 
#'   high \eqn{H^2_{jk}}. Therefore, Friedman and Popescu (2008) suggests to calculate 
#'   \eqn{H^2_{jk}} only for *important* variables (see "Modification" below).
#'   
#' **Modification**
#' 
#' To be better able to compare pairwise interaction strength across variable pairs, 
#' and to overcome the problem mentioned in the last remark, we suggest as alternative 
#' the unnormalized test statistic on the scale of the predictions, 
#' i.e., \eqn{\sqrt{A_{jk}}}. Set `normalize = FALSE` and `squared = FALSE` to obtain
#' this statistic.
#' Furthermore, we do pairwise calculations not for the most *important* features but 
#' rather for those features with *strongest overall interactions*.
#' 
#' @inheritParams h2_overall
#' @inherit h2_overall return
#' @inherit hstats references
#' @export
#' @seealso [hstats()], [h2()], [h2_overall()], [h2_threeway()]
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' s <- hstats(fit, X = iris[-1])
#' 
#' # Proportion of joint effect coming from pairwise interaction
#' # (for features with strongest overall interactions)
#' h2_pairwise(s)
#' h2_pairwise(s, zero = FALSE)  # Drop 0
#' 
#' # Absolute measure as alternative
#' abs_h <- h2_pairwise(s, normalize = FALSE, squared = FALSE, zero = FALSE)
#' abs_h
#' abs_h$M
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' s <- hstats(fit, X = iris[3:5], verbose = FALSE)
#' h2_pairwise(s)
#' h2_pairwise(s, zero = FALSE)
h2_pairwise <- function(object, ...) {
  UseMethod("h2_pairwise")
}

#' @describeIn h2_pairwise Default pairwise interaction strength.
#' @export
h2_pairwise.default <- function(object, ...) {
  stop("No default method implemented.")
}

#' @describeIn h2_pairwise Pairwise interaction strength from "hstats" object.
#' @export
h2_pairwise.hstats <- function(object, normalize = TRUE, squared = TRUE,
                               sort = TRUE, zero = TRUE, eps = 1e-8, ...) {
  get_hstat_matrix(
    statistic = "h2_pairwise",
    object = object,
    normalize = normalize, 
    squared = squared, 
    sort = sort,
    zero = zero,
    eps = eps
  )
}

#' Raw H2 Pairwise
#' 
#' Internal helper function that calculates numerator and denominator of
#' statistic in title.
#' 
#' @noRd
#' @keywords internal
#' @param x A list containing the elements "combs2", "v_pairwise_0", "K", "pred_names", 
#'   "F_jk", "F_j", "eps", and "w".
#' @returns A list with the numerator and denominator statistics.
h2_pairwise_raw <- function(x) {
  num <- init_numerator(x, way = 2L)
  denom <- num + 1
    
  # Note that F_jk are in the same order as x[["combs2"]]
  combs <- x[["combs2"]]
  if (!is.null(combs)) {
    for (nm in names(combs)) {
      z <- combs[[nm]]
      num[nm, ] <- with(
        x, wcolMeans((F_jk[[nm]] - F_j[[z[1L]]] - F_j[[z[2L]]])^2, w = w)
      )
      denom[nm, ] <- with(x, wcolMeans(F_jk[[nm]]^2, w = w))
    }    
  }
  num <- zap_small(num, eps = x[["eps"]])  # Numeric precision
  
  list(num = num, denom = denom)
}
