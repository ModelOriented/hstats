test_that("rep_each() works", {
  expect_equal(rep_each(3, 10), rep_each(3L, 10L))
  expect_equal(rep_each(3, 10), rep(1:3, each = 10))
  expect_true(is.integer(rep_each(100, 100)))
})

test_that("fdummy() works", {
  x <- c("A", "A", "C", "D")
  mm <- matrix(model.matrix(~ x + 0), ncol = 3, dimnames = list(NULL, c("A", "C", "D")))
  expect_equal(fdummy(x), mm)
})

test_that("fdummy() works for singletons", {
  x <- c("A")
  expect_equal(fdummy(x), cbind(A = 1))
  expect_true(is.matrix(fdummy(x)))
})

test_that("fdummy() respects factor level order", {
  x1 <- factor(c("A", "A", "C", "D"))
  x2 <- factor(x1, levels = rev(levels(x1)))
  d1 <- fdummy(x1)
  d2 <- fdummy(x2)
  expect_equal(d1, d2[, colnames(d1)])
  expect_equal(colnames(d1), rev(colnames(d2)))
})

test_that("colMeans_factor() works", {
  x <- c("A", "A", "C", "D")
  expect_equal(colMeans_factor(x), colMeans(fdummy(x)))

  expect_equal(colMeans_factor(c("A", "B", "B")), c(A = 1/3, B = 2/3))
  expect_equal(
    colMeans_factor(factor(c("A", "B", "B"), levels = c("B", "A", "C"))), 
    c(B = 2/3, A = 1/3, C = 0)
  )
  expect_equal(
    colMeans_factor(iris$Species), c(setosa = 1/3, versicolor = 1/3, virginica = 1/3)
  )
  expect_equal(
    colMeans_factor(iris$Species[101:150]), c(setosa = 0, versicolor = 0, virginica = 1)
  )
})

test_that("colMeans_factor() works for singletons", {
  x <- c("A")
  expect_equal(colMeans_factor(x), colMeans(fdummy(x)))
  expect_true(is.vector(colMeans_factor(x)))
})

test_that("colMeans_factor() respects factor level order", {
  x1 <- factor(c("A", "A", "C", "D"))
  x2 <- factor(x1, levels = rev(levels(x1)))
  d1 <- colMeans_factor(x1)
  d2 <- colMeans_factor(x2)
  expect_equal(d1, d2[names(d1)])
  expect_equal(names(d1), rev(names(d2)))
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

test_that("wrowmean_vector() works for vectors and 1D matrices", {
  x1 <- 6:1
  x2 <- cbind(a = x1)
  out1 <- wrowmean_vector(x1, ngroups = 2L)
  out2 <- wrowmean_vector(x2, ngroups = 2L)
  
  expect_error(wrowmean_vector(factor(1, 2)))
  expect_error(wrowmean_vector(matrix(1:4, ncol = 2L)))
  
  expect_true(is.matrix(out1))
  expect_equal(out1, unname(out2))
  expect_equal(out2, cbind(a = c(5, 2)))
  
  expect_equal(out1, wrowmean(x1, ngroups = 2L))
  expect_equal(out2, wrowmean(x2, ngroups = 2L))
  
  expect_equal(wrowmean_vector(x1, ngroups = 3L), cbind(c(5.5, 3.5, 1.5)))
  
  # Constant weights have no effect
  expect_equal(wrowmean_vector(x1, ngroups = 2L, w = c(1, 1, 1)), out1)
  expect_equal(wrowmean_vector(x2, ngroups = 2L, w = c(4, 4, 4)), out2)
  
  # Non-constant weights
  a <- weighted.mean(6:4, 1:3)
  b <- weighted.mean(3:1, 1:3)
  out1 <- wrowmean_vector(x1, ngroups = 2L, w = 1:3)
  out2 <- wrowmean_vector(x2, ngroups = 2L, w = 1:3)
  
  expect_equal(out1, unname(out2))
  expect_equal(out2, cbind(a = c(a, b)))
  expect_equal(out1, wrowmean(x1, ngroups = 2L, w = 1:3))
  expect_equal(out2, wrowmean(x2, ngroups = 2L, w = 1:3))
})

test_that("rowmean_factor() works for factor input", {
  x <- factor(c("C", "A", "C", "C", "A", "A"))
  out <- rowmean_factor(x, ngroups = 2L)
  
  expect_true(is.matrix(out))
  expect_equal(out, cbind(A = c(1/3, 2/3), C = c(2/3, 1/3)))
  expect_equal(out, wrowmean_matrix(fdummy(x), ngroups = 2L))
  expect_equal(out, wrowmean(x, ngroups = 2L))
})

test_that("rowmean() works for factor input with weights", {
  x <- factor(c("C", "A", "C", "C", "A", "A"))
  out <- wrowmean(x, ngroups = 2L)
  
  # Constant weights have no effect
  expect_equal(wrowmean(x, ngroups = 2L, w = c(1, 1, 1)), out)
  expect_equal(wrowmean(x, ngroups = 2L, w = c(4, 4, 4)), out)
  
  # Non-constant weights
  A1 <- weighted.mean(x[1:3] == "A", 1:3)
  A2 <- weighted.mean(x[4:6] == "A", 1:3)
  C1 <- weighted.mean(x[1:3] == "C", 1:3)
  C2 <- weighted.mean(x[4:6] == "C", 1:3)
  expect_equal(wrowmean(x, ngroups = 2L, w = 1:3), cbind(A = c(A1, A2), C = c(C1, C2)))
})

test_that("wrowmean_matrix() works for matrices with > 1 columns", {
  x <- cbind(x = 6:1, z = 1:6)
  out <- wrowmean_matrix(x, ngroups = 2L)
  
  expect_true(is.matrix(out))
  expect_equal(out, cbind(x = c(5, 2), z = c(2, 5)))
  expect_equal(
    wrowmean_matrix(x, ngroups = 3L), 
    cbind(x = c(5.5, 3.5, 1.5), z = c(1.5, 3.5, 5.5))
  )
  expect_equal(out, wrowmean(x, ngroups = 2L))
  
  # Constant weights have no effect
  expect_equal(wrowmean_matrix(x, ngroups = 2L, w = c(1, 1, 1)), out)
  expect_equal(wrowmean_matrix(x, ngroups = 2L, w = c(4, 4, 4)), out)
  
  # Non-constant weights
  xpected <- cbind(
    x = c(weighted.mean(6:4, 1:3), weighted.mean(3:1, 1:3)),
    z = c(weighted.mean(1:3, 1:3), weighted.mean(4:6, 1:3))
  )
  
  out <- wrowmean_matrix(x, ngroups = 2L, w = 1:3)
  expect_equal(out, xpected)
  expect_equal(out, wrowmean(x, ngroups = 2L, w = 1:3))
})

test_that("wrowmean() checks for correct weight lengths", {
  expect_error(wrowmean(1:10, ngroups = 2L, w = 1:10))
})

test_that("wrowmean() equals wColMeans() for ngroups = 1", {
  x <- 1:10
  expect_equal(wrowmean(x), rbind(wcolMeans(x)))
  expect_equal(wrowmean(x, w = 1:10), rbind(wcolMeans(x, w = 1:10)))
  
  X <- cbind(x = x)
  expect_equal(wrowmean(X), rbind(wcolMeans(X)))
  expect_equal(wrowmean(X, w = 1:10), rbind(wcolMeans(X, w = 1:10)))
  
  X <- cbind(a = 1:10, b = 10:1)
  expect_equal(wrowmean(X), rbind(wcolMeans(X)))
  expect_equal(wrowmean(X, w = 1:10), rbind(wcolMeans(X, w = 1:10)))
  
  x <- factor(c("B", "B", "B", "C"))
  expect_equal(wrowmean(x), rbind(wcolMeans(x)))
  expect_equal(wrowmean(x, w = 1:4), rbind(wcolMeans(x, w = 1:4)))
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

  # Grouped and weighted
  r <- gwColMeans(x, g = g, w = w2)
  expect_equal(r$M[2L, ], wcolMeans(x[g == 2, ], w = w2[g == 2]))
  expect_equal(r$w, c(sum(3:6), sum(1:2)))
})

test_that("gwcolMeans() works for factors", {
  x <- factor(c("A", "B", "A", "B", "A", "A"), levels = c("B", "A"))
  g <- c(2, 2, 1, 1, 1, 1)
  w1 <- rep(2, times = 6)
  w2 <- 1:6
  
  # Ungrouped
  r <- gwColMeans(x)
  expect_equal(r$M, rbind(wcolMeans(x)))
  expect_equal(r$w, length(x))
  
  r <- gwColMeans(x, w = w1)
  expect_equal(r$M, rbind(wcolMeans(x, w = w1)))
  expect_equal(r$w, sum(w1))
  
  r <- gwColMeans(x, w = w2)
  expect_equal(r$M, rbind(wcolMeans(x, w = w2)))
  expect_equal(r$w, sum(w2))
  
  # Grouped
  r <- gwColMeans(x, g = g)
  expect_equal(r$M[2L, ], wcolMeans(x[g == 2]))
  expect_equal(r$w, c(4, 2))
  
  # Grouped and weighted
  r <- gwColMeans(x, g = g, w = w2)
  expect_equal(r$M[2L, ], wcolMeans(x[g == 2], w = w2[g == 2]))
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
