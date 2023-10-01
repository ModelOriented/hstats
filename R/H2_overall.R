#' Overall Interaction Strength
#' 
#' Friedman and Popescu's statistic of overall interaction strength per 
#' feature, see Details. Use `plot()` to get a barplot.
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
#' Correspondingly, Friedman and Popescu's statistic of overall interaction 
#' strength is given by
#' \deqn{
#'   H_j^2 = \frac{\frac{1}{n} \sum_{i = 1}^n\big[F(\mathbf{x}_i) - 
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
#' 5. Due to (typically undesired) extrapolation effects of partial dependence functions, 
#'   depending on the model, values above 1 may occur.
#' 6. \eqn{H^2_j = 0} means there are no interactions associated with \eqn{x_j}. 
#'   The higher the value, the more prediction variability comes from interactions 
#'   with \eqn{x_j}.
#' 7. Since the denominator is the same for all features, the values of the test 
#'   statistics can be compared across features.
#' 
#' @param object Object of class "hstats".
#' @param normalize Should statistics be normalized? Default is `TRUE`.
#' @param squared Should *squared* statistics be returned? Default is `TRUE`. 
#' @param sort Should results be sorted? Default is `TRUE`.
#'   (Multioutput is sorted by row means.)
#' @param top_m How many rows should be shown? (`Inf` to show all.)
#' @param zero Should rows with all 0 be shown? Default is `TRUE`.
#' @param eps Threshold below which numerator values are set to 0.
#' @param plot Should results be plotted as barplot? Default is `FALSE`.
#' @param fill Color of bar (only for univariate statistics).
#' @param ... Further parameters passed to `geom_bar()`.
#' @returns 
#'   An object of class "hstats_matrix" containing these elements:
#'   - `M`: Matrix of statistics (one column per prediction dimension), or `NULL`.
#'   - `normalize`: Same as input `normalize`.
#'   - `squared`: Same as input `squared`.
#'   - `statistic`: Name of the statistic.
#'   - `description`: Description of the statistic.
#' @inherit hstats references
#' @seealso [hstats()], [h2()], [h2_pairwise()], [h2_threeway()]
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' s <- hstats(fit, X = iris[-1])
#' h2_overall(s)
#' h2_overall(s, plot = TRUE)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' s <- hstats(fit, X = iris[3:5], verbose = FALSE)
#' h2_overall(s, plot = TRUE, zero = FALSE)
h2_overall <- function(object, ...) {
  UseMethod("h2_overall")
}

#' @describeIn h2_overall Default method of overall interaction strength.
#' @export
h2_overall.default <- function(object, ...) {
  stop("No default method implemented.")
}

#' @describeIn h2_overall Overall interaction strength from "hstats" object.
#' @export
h2_overall.hstats <- function(object, normalize = TRUE, squared = TRUE, 
                              sort = TRUE, zero = TRUE, eps = 1e-8, ...) {
  get_hstat_matrix(
    statistic = "h2_overall",
    object = object,
    normalize = normalize, 
    squared = squared, 
    sort = sort,
    zero = zero,
    eps = eps
  )
}

# Helper function

#' Raw H2 Overall
#' 
#' Internal helper function that calculates numerator and denominator of
#' statistic in title.
#' 
#' @noRd
#' @keywords internal
#' @param x A list containing the elements "v", "K", "pred_names", 
#'   "f", "F_not_j", "F_j", "mean_f2", and "w".
#' @returns A list with the numerator and denominator statistics.
h2_overall_raw <- function(x) {
  num <- init_numerator(x, way = 1L)
  for (z in x[["v"]]) {
    num[z, ] <- with(x, wcolMeans((f - F_j[[z]] - F_not_j[[z]])^2, w = w))
  }
  list(num = num, denom = x[["mean_f2"]])
}
