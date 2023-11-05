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
  
  expect_equal(length(r0), 10L)
  expect_equal(length(r1), 20L)
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

x <- c(NA, 1:98, NA)
y <- c(rep(c("A", "B"), each = 48), c(NA, NA, NA, NA))
xy <- data.frame(x = x, y = y)

test_that("univariate_grid() can deal with missings", {
  expect_true(
    !anyNA(univariate_grid(x, grid_size = 3, strategy = "uniform", na.rm = TRUE))
  )
  expect_true(
    !anyNA(univariate_grid(x, grid_size = 3, strategy = "quantile", na.rm = TRUE))
  )
  expect_true(
    anyNA(univariate_grid(x, grid_size = 3, strategy = "uniform", na.rm = FALSE))
  )
  expect_true(
    anyNA(univariate_grid(x, grid_size = 3, strategy = "quantile", na.rm = FALSE))
  )
  expect_false(
    anyNA(univariate_grid(na.omit(x), grid_size = 3, strategy = "uniform", na.rm = FALSE))
  )
  expect_false(
    anyNA(univariate_grid(na.omit(x), grid_size = 3, strategy = "quantile", na.rm = FALSE))
  )
  
  expect_true(!anyNA(univariate_grid(y, na.rm = TRUE)))
  expect_true(anyNA(univariate_grid(y, na.rm = FALSE)))
  expect_false(anyNA(univariate_grid(na.omit(y), na.rm = FALSE)))
})

test_that("multivariate_grid() can deal with missings", {
  expect_true(
    !anyNA(multivariate_grid(xy, grid_size = 6, strategy = "uniform", na.rm = TRUE))
  )
  expect_false(
    !anyNA(multivariate_grid(xy, grid_size = 6, strategy = "uniform", na.rm = FALSE))
  )
  expect_false(
    anyNA(multivariate_grid(na.omit(xy), grid_size = 6, strategy = "uniform", na.rm = FALSE))
  )
})

test_that("qcut() works (test should be improved)", {
  x <- 1:100
  expect_equal(levels(qcut(x, m = 2)), c("[1,50]", "(50,100]"))
  expect_equal(levels(qcut(x, m = 4)), c("[1,25]", "(25,50]", "(50,75]", "(75,100]"))
})

test_that("qcut() works with missings", {
  expect_true(is.na(qcut(c(NA, 1:9), m = 2)[1L]))
})

test_that("approx_matrix_or_df works as expected", {
  expect_equal(approx_matrix_or_df(iris, m = 200L), iris)
  expect_false(identical(r <- approx_matrix_or_df(iris, m = 5L), iris))
  expect_equal(length(unique(r$Species)), 3L)
  expect_equal(length(unique(r$Sepal.Width)), 5L)
  
  ir <- data.matrix(iris[1:4])
  expect_equal(approx_matrix_or_df(ir, m = 200L), ir)
  expect_false(identical(approx_matrix_or_df(ir, m = 5L), ir))
  expect_equal(length(unique(r[, "Sepal.Width"])), 5L)
  
  X <- cbind(dense = 1:20, discrete = rep(1:2, each = 10))
  expect_equal(
    apply(approx_matrix_or_df(X, m = 5L), 2L, function(x) length(unique(x))), 
    c(dense = 5L, discrete = 2L)
  )
})

test_that("approx_vector() works with missings", {
  expect_equal(approx_vector(c(NA, "A", "B"), m = 2), c(NA, "A", "B"))
  expect_true(is.na(approx_vector(c(NA, 1:9), m = 2)[1L]))
})
