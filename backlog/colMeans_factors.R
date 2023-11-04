#' colMeans() for Factors
#'
#' Internal function used to calculate proportions of factor levels.
#' It is less memory-hungry than `colMeans(fdummy(x))`, and much faster.
#' A weighted version via `rowsum(w, x)` is not consistently faster than
#' `wcolMeans(fdummy(x))`, so we currently focus on the non-weighted case.
#' Furthermore, `rowsum()` drops empty factor levels.
#'
#' @noRd
#' @keywords internal
#'
#' @params x Factor.
#' @returns Named vector.
colMeans_factor <- function(x) {
  x <- as.factor(x)
  lev <- levels(x)
  out <- tabulate(x, nbins = length(lev)) / length(x)
  names(out) <- lev
  out
}

library(testthat)

test_that("colMeans_factor() matches some results", {
  expect_equal(colMeans_factor(c("A", "B", "B")), c(A = 1/3, B = 2/3))
  expect_equal(colMeans_factor(factor(c("A", "B", "B"), levels = c("B", "A", "C"))), c(B = 2/3, A = 1/3, C = 0))
  expect_equal(colMeans_factor(iris$Species), c(setosa = 1/3, versicolor = 1/3, virginica = 1/3))
  expect_equal(colMeans_factor(iris$Species[101:150]), c(setosa = 0, versicolor = 0, virginica = 1))
})
