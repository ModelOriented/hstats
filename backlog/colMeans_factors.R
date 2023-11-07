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

wrowmean_matrix2 <- function(x, ngroups = 1L, w = NULL) {
  if (!is.matrix(x)) {
    stop("x must be a matrix.")
  }
  dim(x) <- c(nrow(x)/ngroups, ngroups, ncol(x))
  out <- colMeans(aperm(x, c(1, 3, 2)))
  t.default(out)
}
