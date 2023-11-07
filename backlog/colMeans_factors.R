#' gcolMeans() for Factors
#'
#' Grouped version of `colMeans_factor()`.
#'
#' @noRd
#' @keywords internal
#'
#' @params x Factor.
#' @returns Named vector.
gcolMeans_factor <- function(x, g = NULL) {
  if (is.null(g)) {
    colMeans_factor(x)
  }
  x <- as.factor(x)
  out <- t.default(sapply(split.default(x, g), colMeans_factor))
  colnames(out) <- levels(x)
  out
}

