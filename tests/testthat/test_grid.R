set.seed(1L)
n <- 1000
x <- rexp(n)

test_that("univariate_grid() reacts on strategy", {
  r0 <- univariate_grid(x, strategy = "quantile")
  r1 <- univariate_grid(x, strategy = "uniform")
  
  expect_true(!identical(r0, r1))
})

test_that("univariate_grid() mode 'uniform' reacts on 'trim'", {
  r0 <- univariate_grid(x, trim = 0:1)
  r1 <- univariate_grid(x, trim = c(0, 0.8))
  expect_true(max(r1) < max(r0))
})

test_that("univariate_grid() mode 'quantile' reacts on 'trim'", {
  r0 <- univariate_grid(x, trim = 0:1, strategy = "quantile")
  r1 <- univariate_grid(x, trim = c(0, 0.8), strategy = "quantile")
  
  expect_true(max(r1) < max(r0))
})

test_that("univariate_grid() reacts on grid_size (mode 'quantile')", {
  r0 <- univariate_grid(x, grid_size = 10L, strategy = "quantile")
  r1 <- univariate_grid(x, grid_size = 20L, strategy = "quantile")
  
  expect_equal(length(r0), 10L)
  expect_equal(length(r1), 20L)
})

test_that("univariate_grid() reacts on grid_size (mode 'uniform')", {
  r0 <- univariate_grid(x, grid_size = 10L, strategy = "uniform")
  r1 <- univariate_grid(x, grid_size = 20L, strategy = "uniform")
  
  expect_true(length(r0) < length(r1))
})

test_that("multivariate_grid() equals univariate_grid() for univariate input", {
  r0 <- univariate_grid(x)
  r1 <- multivariate_grid(x)
  
  expect_equal(r0, r1)
})

test_that("multivariate_grid() returns exact size in simple case and 'quantile' mode", {
  p <- 2L
  r1 <- multivariate_grid(iris[seq_len(p)], grid_size = p^2, strategy = "quantile")
  expect_equal(nrow(r1), p^2)
})

test_that("multivariate_grid() is consistent with univariate_grid() - mode univariate", {
  r0 <- multivariate_grid(iris[1:2], grid_size = 4^2)
  r1 <- univariate_grid(iris[, 1L], grid_size = 4)
  r2 <- univariate_grid(iris[, 2L], grid_size = 4)
  
  expect_equal(r1, unique(r0[, 1L]))
  expect_equal(r2, unique(r0[, 2L]))
})

test_that("multivariate_grid() is consistent with univariate_grid() - mode quantile", {
  r0 <- multivariate_grid(iris[4:5], grid_size = 4^2, strategy = "quantile")
  r1 <- univariate_grid(iris[, 4L], grid_size = 4, strategy = "quantile")
  r2 <- univariate_grid(iris[, 5L], grid_size = 4, strategy = "quantile")
  
  expect_equal(r1, unique(r0[, 1L]))
  expect_equal(r2, unique(r0[, 2L]))
})

test_that("multivariate_grid() works for matrix input", {
  r0 <- multivariate_grid(iris[2:3])
  r1 <- multivariate_grid(data.matrix(iris[2:3]))
  expect_true(is.matrix(r1))
  expect_equal(as.matrix(r0), r1)
})

test_that("check_grid() fires on some bad input", {
  r0 <- multivariate_grid(iris[1:2])
  v <- colnames(iris[1:2])
  expect_error(check_grid(r0, v = "Species"))
  expect_error(check_grid(r0[1L], v = "Sepal.Width"))
  expect_error(check_grid(r0, v = v, X_is_matrix = TRUE))
  expect_no_error(check_grid(r0, v = v, X_is_matrix = FALSE))
})
