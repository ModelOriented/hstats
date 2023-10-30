test_that("align_pred() works", {
  expect_error(align_pred("a"))
  
  x <- array(1:4, dim = c(2L, 2L))
  
  expect_equal(align_pred(x), cbind(1:2, 3:4))
  expect_equal(align_pred(1:4), cbind(1:4))
})

test_that("wrowmean() works for vector input", {
  x <- 6:1
  out <- wrowmean(x, ngroups = 2L)
  expect_true(is.matrix(out))
  expect_equal(as.vector(out), c(5, 2))
  expect_equal(as.vector(wrowmean(x, ngroups = 3L)), c(5.5, 3.5, 1.5))
  
  # Constant weights have no effect
  expect_equal(wrowmean(x, ngroups = 2L, w = c(1, 1, 1)), out)
  expect_equal(wrowmean(x, ngroups = 2L, w = c(4, 4, 4)), out)
  
  # Non-constant weights
  a <- weighted.mean(6:4, 1:3)
  b <- weighted.mean(3:1, 1:3)
  expect_equal(as.vector(wrowmean(x, ngroups = 2L, w = 1:3)), c(a, b))
})

test_that("wrowmean() works for matrix input", {
  x <- cbind(x = 6:1, z = 1:6)
  out <- wrowmean(x, ngroups = 2L)
  expect_true(is.matrix(out))
  expect_equal(out, cbind(x = c(5, 2), z = c(2, 5)))
  expect_equal(
    wrowmean(x, ngroups = 3L), 
    cbind(x = c(5.5, 3.5, 1.5), z = c(1.5, 3.5, 5.5))
  )
  
  # Constant weights have no effect
  expect_equal(wrowmean(x, ngroups = 2L, w = c(1, 1, 1)), out)
  expect_equal(wrowmean(x, ngroups = 2L, w = c(4, 4, 4)), out)
  
  # Non-constant weights
  xpected <- cbind(
    x = c(weighted.mean(6:4, 1:3), weighted.mean(3:1, 1:3)),
    z = c(weighted.mean(1:3, 1:3), weighted.mean(4:6, 1:3))
  )
  
  expect_equal(wrowmean(x, ngroups = 2L, w = 1:3), xpected)
})

test_that("wcolMeans() works", {
  x <- cbind(a = 1:6, b = 6:1)
  x_df <- as.data.frame(x)
  expect_equal(wcolMeans(x), colMeans(x))
  expect_equal(wcolMeans(x_df), colMeans(x_df))
  expect_equal(wcolMeans(x_df$a), mean(x_df$a))
  
  # Weighted case
  expect_equal(wcolMeans(x, w = rep(2, times = 6L)), colMeans(x))
  
  w <- c(1, 1, 2, 2, 3, 3)
  xpected <- c(a = weighted.mean(1:6, w), b = weighted.mean(6:1, w))
  expect_equal(wcolMeans(x, w = w), xpected)
  
  expect_equal(wcolMeans(x_df$a, w = w), weighted.mean(x_df$a, w = w))
})

test_that("gwcolMeans() works", {
  x <- cbind(a = 1:6, b = 6:1)
  g <- c(2, 2, 1, 1, 1, 1)
  w1 <- rep(2, times = 6)
  w2 <- 1:6
  
  # Ungrouped
  r <- gwColMeans(x)
  expect_equal(r$M, rbind(wcolMeans(x)))
  expect_equal(r$w, nrow(x))
  
  r <- gwColMeans(x, w = w1)
  expect_equal(r$M, rbind(wcolMeans(x, w = w1)))
  expect_equal(r$w, sum(w1))
  
  r <- gwColMeans(x, w = w2)
  expect_equal(r$M, rbind(wcolMeans(x, w = w2)))
  expect_equal(r$w, sum(w2))
  
  # Grouped
  r <- gwColMeans(x, g = g)
  expect_equal(r$M[2L, ], wcolMeans(x[g == 2, ]))
  expect_equal(r$w, c(4, 2))
  
  r <- gwColMeans(x, g = g, reorder = FALSE)
  expect_equal(r$M[2L, ], wcolMeans(x[g == 1, ]))
  expect_equal(r$w, c(2, 4))

  # Grouped and weighted
  r <- gwColMeans(x, g = g, w = w2)
  expect_equal(r$M[2L, ], wcolMeans(x[g == 2, ], w = w2[g == 2]))
  expect_equal(r$w, c(sum(3:6), sum(1:2)))
})

test_that("wcenter() works for matrices with > 1 columns", {
  x <- cbind(a = 1:6, b = 6:1)
  expect_equal(wcenter(x), cbind(a = 1:6 - mean(1:6), b = 6:1 - mean(6:1)))

  # Weighted case
  expect_equal(wcenter(x), wcenter(x, w = rep(2, 6L)))
  
  w <- c(1, 1, 2, 2, 3, 3)
  xpected <- cbind(a = 1:6 - weighted.mean(1:6, w), b = 6:1 - weighted.mean(6:1, w))
  expect_equal(wcenter(x, w = w), xpected)
})

test_that("wcenter() works for matrices with 1 column", {
  x <- cbind(a = 1:6)
  expect_equal(wcenter(x), cbind(a = 1:6 - mean(1:6)))
  
  # Weighted case
  expect_equal(wcenter(x), wcenter(x, w = rep(2, 6L)))
  
  w <- c(1, 1, 2, 2, 3, 3)
  xpected <- cbind(a = 1:6 - weighted.mean(1:6, w))
  expect_equal(wcenter(x, w = w), xpected)
})

test_that("wcenter() works for vectors", {
  x <- 1:6
  expect_equal(wcenter(x), cbind(1:6 - mean(1:6)))
  
  # Weighted case
  expect_equal(wcenter(x), wcenter(x, w = rep(2, 6L)))
  
  w <- c(1, 1, 2, 2, 3, 3)
  xpected <- cbind(1:6 - weighted.mean(1:6, w))
  expect_equal(wcenter(x, w = w), xpected)
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
