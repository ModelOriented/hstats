#' Overall Interaction Strength
#' 
#' Friedman and Popescu's \eqn{H^2_j} statistics of overall interaction strength per 
#' feature extracted from the result of [interact()], see Details.
#' 
#' @details
#' The logic of Friedman and Popescu (2008) is as follows: 
#' If there are no interactions involving feature \eqn{x_j}, we can decompose the 
#' (centered) prediction function \eqn{F} into the sum of the (centered) partial 
#' dependence \eqn{F_j} on \eqn{x_j} and the (centered) partial dependence 
#' \eqn{F_{\setminus j}} on all other features \eqn{\mathbf{x}_{\setminus j}}, i.e.,
#' \deqn{
#'   F(\mathbf{x}) = F_j(x_j) + F_{\setminus j}(\mathbf{x}_{\setminus j}).
#' }
#' Correspondingly, Friedman and Popescu's \eqn{H^2_j} statistic of overall interaction 
#' strength is given by
#' \deqn{
#'   H_{j}^2 = \frac{\frac{1}{n} \sum_{i = 1}^n\big[F(\mathbf{x}_i) - 
#'   \hat F_j(x_{ij}) - \hat F_{\setminus j}(\mathbf{x}_{i\setminus j})
#'   \big]^2}{\frac{1}{n} \sum_{i = 1}^n\big[F(\mathbf{x}_i)\big]^2}
#' }
#' (check [partial_dep()] for all definitions).
#' 
#' **Remarks:**
#' 
#' 1. Partial dependence functions (and \eqn{F}) are all centered to 
#'   (possibly weighted) mean 0.
#' 2. Partial dependence functions (and \eqn{F}) are evaluated over the data distribution. 
#'   This is different to partial dependence plots, where one uses a fixed grid.
#' 3. Weighted versions follow by replacing all arithmetic means by corresponding
#'   weighted means.
#' 4. Multivariate predictions can be treated in a component-wise manner.
#' 5. \eqn{H^2_j = 0} means there are no interactions associated with \eqn{x_j}. 
#'   The higher the value, the more prediction variability comes from interactions 
#'   with \eqn{x_j}.
#' 6. Since the denominator is the same for all features, the values of the test 
#'   statistics can be compared across features.
#' 
#' @param object Object of class "interact".
#' @param normalize Should statistic be normalized? Default is `TRUE`.
#' @param squared Should *squared* statistics be returned? Default is `TRUE`. 
#' @param sort Should results be sorted by the size of the statistic? Default is `TRUE`.
#'   Multioutput predictions are sorted by row means.
#' @param top_m How many statistics should be shown? By default `Inf` (show all).
#' @param eps Threshold below which numerator values are set to 0.
#' @param ... Further parameters passed to predict function (only for default method).
#' @returns 
#'   Matrix of interactions statistics (one row per variable, one column per
#'   prediction dimension).
#' @inherit interact references
#' @seealso [interact()], [H2()], [H2_jk()]
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' H2_j(inter)
#' 
#' # As a barplot
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(inter, stat = 1)
#' }
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' H2_j(inter)
#' 
#' # As a barplot
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(inter, stat = 1)
#' }
H2_j <- function(object, ...) {
  UseMethod("H2_j")
}

#' @describeIn H2_j Default method of overall interaction strength.
#' @export
H2_j.default <- function(object, ...) {
  stop("No default method implemented.")
}

#' @describeIn H2_j Overall interaction strength from "interact" object.
#' @export
H2_j.interact <- function(object, normalize = TRUE, squared = TRUE, sort = TRUE, 
                          top_m = Inf, eps = 1e-8, ...) {
  H2_j_raw(
    F_j = object[["F_j"]],
    F_not_j = object[["F_not_j"]], 
    f = object[["f"]], 
    mean_f2 = object[["mean_f2"]], 
    w = object[["w"]],
    normalize = normalize,
    squared = squared,
    sort = sort,
    top_m = top_m,
    eps = eps
  )
}


#' Raw H2 Overall
#' 
#' Function used to calculate Friedman and Popescu's overall H-squared. It is separated
#' from the main H2_j function because it is used within interaction_statistics
#' to select the variables for pairwise calculations.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param F_j List of empirical PD values per feature.
#' @param F_not_j List of empirical PD values of all features except j.
#' @param f Predictions.
#' @param mean_f2 Weighted average of f^2.
#' @param w Optional case weights.
#' @inheritParams H2_j
H2_j_raw <- function(F_j, F_not_j, f, mean_f2, w = NULL, normalize = TRUE, 
                     squared = TRUE, sort = TRUE, top_m = Inf, eps = 1e-8, ...) {
  v <- names(F_j)
  num <- matrix(nrow = length(v), ncol = ncol(f), dimnames = list(v, colnames(f)))
  for (z in v) {
    num[z, ] <- wcolMeans((f - F_j[[z]] - F_not_j[[z]])^2, w = w)
  }
  postprocess(
    num = num,
    denom = mean_f2,
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}
