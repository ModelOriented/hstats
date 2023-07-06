#' Three-way Interaction Strength
#' 
#' Friedman and Popescu's statistics of three-way interaction strength extracted from the
#' result of [interact()], see Details. By default, the results are plotted as a
#' barplot. Set `plot = FALSE` to get a matrix of values instead.
#' 
#' @inheritParams H2_overall
#' @returns 
#'   A "ggplot" object (if `plot = TRUE`), or a matrix of statistics 
#'   (one row per variable, one column per prediction dimension). If no pairwise
#'   statistics have been calculated, the function returns `NULL`.
#' @inherit interact references
#' @export
#' @seealso [interact()], [H2()], [H2_overall()], [H2_pairwise()]
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # Proportion of joint effect coming from three-way interaction
#' # (for features with strongest overall interactions)
#' H2_threeway(inter)
#' H2_threeway(inter, plot = FALSE)
#' 
#' # Absolute measure as alternative
#' H2_threeway(inter, normalize = FALSE, squared = FALSE)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' H2_threeway(inter)
H2_threeway <- function(object, ...) {
  UseMethod("H2_threeway")
}

#' @describeIn H2_threeway Default pairwise interaction strength.
#' @export
H2_threeway.default <- function(object, ...) {
  stop("No default method implemented.")
}

#' @describeIn H2_threeway Pairwise interaction strength from "interact" object.
#' @export
H2_threeway.interact <- function(object, normalize = TRUE, squared = TRUE, sort = TRUE, 
                                 top_m = 15L, eps = 1e-8, plot = TRUE, fill = "#2b51a1", 
                           ...) {
  combs <- object[["combs3"]]
  
  if (is.null(combs)) {
    return(NULL)
  }
  
  # Note that the F_jkl are in the same order as combs
  num <- denom <- with(
    object,
    matrix(
      nrow = length(combs), ncol = K, dimnames = list(names(combs), pred_names)
    )
  )
  
  for (i in seq_along(combs)) {
    z <- combs[[i]]
    zz <- sapply(utils::combn(z, 2L, simplify = FALSE), paste, collapse = ":")

    num[i, ] <- with(
      object, 
      wcolMeans((F_jkl[[i]] - Reduce("+", F_jk[zz]) + Reduce("+", F_j[z]))^2, w = w)
    )
    denom[i, ] <- if (normalize) with(object, wcolMeans(F_jkl[[i]]^2, w = w)) else 1
  }
  out <- postprocess(
    num = num,
    denom = denom,
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
  if (plot) plot_stat(out, fill = fill, ...) else out
}
