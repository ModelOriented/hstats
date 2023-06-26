test_that("align_pred() works", {
  expect_error(align_pred("a"))
  
  x <- array(1:4, dim = c(2L, 2L))
  
  expect_equal(align_pred(x), cbind(1:2, 3:4))
  expect_equal(align_pred(1:4), cbind(1:4))
})

test_that("rowmean() works for vector input", {
  x <- 6:1
  out <- rowmean(x, ngroups = 2L)
  expect_true(is.matrix(out))
  expect_equal(as.vector(out), c(5, 2))
  expect_equal(as.vector(rowmean(x, ngroups = 3L)), c(5.5, 3.5, 1.5))
  
  # Constant weights have no effect
  expect_equal(rowmean(x, ngroups = 2L, w = c(1, 1, 1)), out)
  expect_equal(rowmean(x, ngroups = 2L, w = c(4, 4, 4)), out)
  
  # Non-constant weights
  a <- weighted.mean(6:4, 1:3)
  b <- weighted.mean(3:1, 1:3)
  expect_equal(as.vector(rowmean(x, ngroups = 2L, w = 1:3)), c(a, b))
})

test_that("rowmean() works for matrix input", {
  x <- cbind(x = 6:1, z = 1:6)
  out <- rowmean(x, ngroups = 2L)
  expect_true(is.matrix(out))
  expect_equal(out, cbind(x = c(5, 2), z = c(2, 5)))
  expect_equal(
    rowmean(x, ngroups = 3L), 
    cbind(x = c(5.5, 3.5, 1.5), z = c(1.5, 3.5, 5.5))
  )
  
  # Constant weights have no effect
  expect_equal(rowmean(x, ngroups = 2L, w = c(1, 1, 1)), out)
  expect_equal(rowmean(x, ngroups = 2L, w = c(4, 4, 4)), out)
  
  # Non-constant weights
  xpected <- cbind(
    x = c(weighted.mean(6:4, 1:3), weighted.mean(3:1, 1:3)),
    z = c(weighted.mean(1:3, 1:3), weighted.mean(4:6, 1:3))
  )
  
  expect_equal(rowmean(x, ngroups = 2L, w = 1:3), xpected)
})

test_that(".compress_X() works for data.frames", {
  # Note that b is not used after compression
  X <- data.frame(a = c(1, 1, 2, 2, 2), b = 1:5)
  out_df <- data.frame(a = c(1, 2), b = c(1, 3), row.names = c(1L, 3L))
  out <- .compress_X(X, v = "b")
  expect_equal(out$X, out_df)
  expect_equal(out$w, c(2, 3))
  
  # Weighted with constants
  w <- rep(2, times = 5)
  out_w <- .compress_X(X, v = "b", w = w)
  expect_equal(out_w$X, out_df)
  expect_equal(out_w$w, 2 * c(2, 3))
  
  # Varying weights
  w <- 5:1
  out_w2 <- .compress_X(X, v = "b", w = w)
  expect_equal(out_w2$X, out_df)
  expect_equal(out_w2$w, c(9, 6))
})

test_that(".compress_X() works for matrices", {
  X <- cbind(a = c(1, 1, 2, 2, 2), b = 1:5)
  out <- .compress_X(X, v = "b")
  out_mat <- cbind(c(1, 2), b = c(1, 3))
  dimnames(out_mat) <- list(NULL, c("a", "b"))
  expect_equal(out$X, out_mat)
  expect_equal(out$w, c(2, 3))
  
  # Weighted with constants
  w <- rep(2, times = 5)
  out_w <- .compress_X(X, v = "b", w = w)
  expect_equal(out_w$X, out_mat)
  expect_equal(out_w$w, 2 * c(2, 3))
  
  # Varying weights
  w <- 5:1
  out_w2 <- .compress_X(X, v = "b", w = w)
  expect_equal(out_w2$X, out_mat)
  expect_equal(out_w2$w, c(9, 6))
})


test_that(".compress_X() leaves X unchanged if unique", {
  X <- data.frame(a = 1:5, b = rep(1, times = 5))
  out <- .compress_X(X, v = "b")
  expect_equal(length(out), 2L)
  expect_equal(out$X, X)
  expect_equal(out$w, NULL)
})

test_that(".compress_X() leaves X unchanged if not exactly 1 non-grid variable", {
  X <- data.frame(a = 1:5, b = rep(1, times = 5), c = rep(2, times = 5))
  out <- .compress_X(X, v = "a")
  expect_equal(length(out), 2L)
  expect_equal(out$X, X)
  expect_equal(out$w, NULL)
})

test_that(".compress_grid() works for vectors", {
  g <- c(5, 5, 1, 1, 1)
  out <- .compress_grid(g)
  expect_equal(out$grid, c(5, 1))
  expect_equal(out$grid[out$reindex], g)
})

test_that(".compress_grid() works for matrices", {
  g <- cbind(a = c(1, 1, 2, 2, 3), b = c(1, 2, 1, 1, 1))
  out <- .compress_grid(g)
  expect_equal(out$grid, cbind(a = c(1, 1, 2, 3), b = c(1, 2, 1, 1)))
  expect_equal(out$grid[out$reindex, ], g)
})

test_that(".compress_grid() works for data.frames", {
  g <- data.frame(a = c(1, 1, 2, 2, 3), b = c(2, 2, 1, 1, 1))
  out <- .compress_grid(g)
  expect_equal(
    out$grid, 
    data.frame(a = c(1, 2, 3), b = c(2, 1, 1), row.names = c(1L, 3L, 5L))
  )
  g_out <- out$grid[out$reindex, ]
  rownames(g_out) <- 1:5
  expect_equal(g_out, g)
})

test_that(".compress_grid() leaves grid unchanged if unique", {
  g <- data.frame(a = 1:5, b = rep(1, times = 5))
  out <- .compress_grid(g)
  expect_equal(length(out), 2L)
  expect_equal(out$grid, g)
  expect_equal(out$reindex, NULL)
})


