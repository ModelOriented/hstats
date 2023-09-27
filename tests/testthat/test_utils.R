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
  expect_equal(gwColMeans(x), rbind(wcolMeans(x)))
  expect_equal(gwColMeans(x, w = w1), rbind(wcolMeans(x, w = w1)))
  expect_equal(gwColMeans(x, w = w2), rbind(wcolMeans(x, w = w2)))
  
  # Grouped
  expect_equal(gwColMeans(x, g = g)[2L, ], wcolMeans(x[g == 2, ]))
  expect_equal(gwColMeans(x, g = g, reorder = FALSE)[2L, ], wcolMeans(x[g == 1, ]))
  
  # Grouped and weighted
  expect_equal(
    gwColMeans(x, g = g, w = w2)[2L, ], 
    wcolMeans(x[g == 2, ], w = w2[g == 2])
  )
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

test_that("basic_checks fire some errors", {
  expect_error(basic_check(X = 1:3, v = "a", pred_fun = predict, w = NULL))
  expect_error(basic_check(X = iris[0], v = "a", pred_fun = predict, w = NULL))
  expect_error(basic_check(X = iris, v = "a", pred_fun = predict, w = NULL))
  expect_error(basic_check(X = iris, v = "Species", pred_fun = "mean", w = NULL))
  expect_error(basic_check(X = iris, v = "Species", pred_fun = predict, w = 1:3))
})

test_that("postprocess() works for matrix input", {
  num <- cbind(a = 1:3, b = c(1, 1, 1))
  denom <- cbind(a = 1:3, b = 1:3)
  
  expect_equal(postprocess(num = num, sort = FALSE), num)
  expect_equal(postprocess(num = num, denom = denom, sort = FALSE), num / denom)
  expect_equal(postprocess(num = num), num[3:1, ])
  expect_equal(postprocess(num = num, sort = FALSE, top_m = 2), num[1:2, ])
  expect_equal(postprocess(num = num, squared = FALSE), sqrt(num[3:1, ]))
  
  expect_equal(postprocess(num = num, denom = 2, sort = FALSE), num / 2)
  expect_equal(
    postprocess(num = num, denom = 1:2, sort = FALSE), 
      num / cbind(c(1, 1, 1), c(2, 2, 2))
  )
  
  expect_equal(postprocess(num = cbind(0:1, 0:1), zero = FALSE), rbind(c(1, 1)))
  expect_null(postprocess(num = cbind(0, 0), zero = FALSE))
})

test_that("postprocess() works for vector input", {
  num <- 1:3
  denom <- c(2, 4, 6)
  
  expect_equal(postprocess(num = num), 3:1)
  expect_equal(postprocess(num = num, denom = denom), num / denom)
  expect_equal(postprocess(num = num, sort = FALSE), num)
  expect_equal(postprocess(num = num, sort = FALSE, top_m = 2), num[1:2])
  expect_equal(postprocess(num = num, squared = FALSE), sqrt(num[3:1]))
  
  expect_equal(postprocess(num = 0:1, denom = c(2, 2), zero = FALSE), 0.5)
  expect_null(postprocess(num = 0, zero = FALSE))
})

test_that(".zap_small() works for vector input", {
  expect_equal(.zap_small(1:3), 1:3)
  expect_equal(.zap_small(c(1:3, NA)), c(1:3, 0))
  expect_equal(.zap_small(c(0.001, 1), eps = 0.01), c(0, 1))
})

test_that("poor_man_stack() works (test could be improved", {
  y <- c("a", "b", "c")
  z <- c("aa", "bb", "cc")
  X <- data.frame(x = 1:3, y = y, z = z)
  out <- poor_man_stack(X, to_stack = c("y", "z"))
  xpected <- data.frame(
    x = rep(1:3, times = 2L), 
    varying_ = rep(c("y", "z"), each = 3L),
    value_ = c(y, z)
  )
  expect_equal(out, xpected)
  
  expect_error(poor_man_stack(cbind(a = 1:3, b = 2:4), to_stack = "b"))
})

test_that("mat2df() works (test could be improved)", {
  mat <- cbind(y = 1:2, z = c(0.5, 0.5))
  rownames(mat) <- letters[seq_len(nrow(mat))]
  out <- mat2df(mat)
  rownames(out) <- NULL
  xpected <- data.frame(
    id_ = "Overall", 
    variable_ = factor(c("a", "b", "a", "b"), levels = c("b", "a")),
    varying_ = c("y", "y", "z", "z"),
    value_ = c(1, 2, 0.5, 0.5),
    stringsAsFactors = FALSE
  )
  expect_equal(out, xpected)
  
  mat_no_names <- mat
  colnames(mat_no_names) <- NULL
  expect_equal(unique(mat2df(mat_no_names)$varying_), c("y1", "y2"))
  
  expect_error(mat2df(head(iris)))
  expect_error(mat2df(1:4))
})

test_that("qcut() works (test should be improved)", {
  x <- 1:100
  expect_equal(levels(qcut(x, m = 2)), c("[1,50]", "(50,100]"))
  expect_equal(levels(qcut(x, m = 4)), c("[1,25]", "(25,50]", "(50,75]", "(75,100]"))
})
