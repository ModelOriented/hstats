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

# Univariate model
X <- data.frame(x1 = 1:6, x2 = c(NA, 1, 2, 1, 1, 3), x3 = factor(c("A", NA, NA, "B", "A", "A")))
y <- 1:6
pf <- function(fit, x) x$x1
fit <- "a model"

test_that("average_loss() works without BY", {
  expect_equal(drop(average_loss(fit, X = X, y = y, pred_fun = pf)$M), 0)
})

test_that("average_loss() works with BY", {
  expect_warning(
    expect_warning(r <- average_loss(fit, X = X, y = y, pred_fun = pf, BY = "x3"))
  )
  expect_equal(unname(drop(r$M)), c(0, 0, 0))
  expect_s3_class(plot(r), "ggplot")
})

test_that("perm_importance() works", {
  set.seed(1L)
  expect_no_error(r <- perm_importance(fit, X = X, y = y, pred_fun = pf))
  expect_true(r$M[1L] > 0 && all(r$M[2:3] == 0))
})

test_that("ice() works when non-v variable contains missing", {
  set.seed(1L)
  expect_no_error(r <- ice(fit, v = "x1", X = X, pred_fun = pf))
  expect_equal(r$data$x1, r$data$y)
})

test_that("ice() works when v contains missing", {
  expect_no_error(r1 <- ice(fit, v = "x2", X = X, pred_fun = pf))
  expect_true(!anyNA(r1$data$x2))
  
  expect_no_error(r2 <- ice(fit, v = "x2", X = X, pred_fun = pf, na.rm = FALSE))
  expect_true(anyNA(r2$data$x2))
  
  expect_equal(r1$data[1:3, ], r2$data[1:3, ])
  expect_s3_class(plot(r2, alpha = 1), "ggplot")
})

test_that("ice() works when v contains missing (multivariate)", {
  v <- c("x2", "x3")
  
  expect_no_error(r1 <- ice(fit, v = v, X = X, pred_fun = pf))
  expect_true(!anyNA(r1$data$x2))
  
  expect_no_error(r2 <- ice(fit, v = v, X = X, pred_fun = pf, na.rm = FALSE))
  expect_true(anyNA(r2$data$x2))
})

test_that("ice() works with missing value in BY", {
  expect_true(anyNA(ice(fit, v = "x1", X = X, pred_fun = pf, BY = "x3")$data$x3))
  r <- ice(fit, v = "x2", X = X, pred_fun = pf, BY = "x3")
  expect_true(anyNA(r$data$x3))
  expect_s3_class(plot(r), "ggplot")
})

test_that("partial_dep() works when non-v variable contains missing", {
  expect_no_error(r <- partial_dep(fit, v = "x1", X = X, pred_fun = pf))
  expect_equal(r$data$x1, r$data$y)
})

test_that("partial_dep() works when v contains missing", {
  expect_no_error(r1 <- partial_dep(fit, v = "x2", X = X, pred_fun = pf, grid_size = 2))
  expect_true(!anyNA(r1$data$x2))
  
  expect_no_error(
    r2 <- partial_dep(fit, v = "x2", X = X, pred_fun = pf, na.rm = FALSE, grid_size = 2)
  )
  expect_true(anyNA(r2$data$x2))
  expect_equal(r1$data, r2$data[1:2, ])
  expect_s3_class(plot(r2), "ggplot")
})

test_that("partial_dep() works when v contains missing (multi)", {
  v <- c("x2", "x3")
  expect_no_error(r1 <- partial_dep(fit, v = v, X = X, pred_fun = pf))
  expect_true(!anyNA(r1$data$x2))
  
  expect_no_error(
    r2 <- partial_dep(fit, v = v, X = X, pred_fun = pf, na.rm = FALSE)
  )
  expect_true(anyNA(r2$data$x2))
  expect_s3_class(plot(r2), "ggplot")
})

test_that("partial_dep() works when BY variable contains missing", {
  expect_no_error(
    r <- partial_dep(fit, v = "x2", X = X, pred_fun = pf, BY = "x3", na.rm = FALSE)
  )
  expect_true(anyNA(r$data$x3))
  expect_s3_class(plot(r), "ggplot")
})

pfi <- function(fit, x) ifelse(is.na(x$x1 * x$x2), 1, x$x1 * x$x2)

test_that("hstats() does not give an error with missing", {
  expect_warning(
    expect_warning(
      expect_warning(
        expect_no_error(
          r <- hstats(fit, X = X, pred_fun = pfi, verbose = FALSE)
        )
      )
    )
  )
  expect_true(drop(r$h2$num) > 0)
  expect_equal(rownames(h2_pairwise(r, zero = FALSE)), "x1:x2")
})

# Some checks on pd_raw()

test_that(".compress_grid() works with missing values in grid", {
  g <- c(2, 2, NA, 1, NA)
  gg <- .compress_grid(g)
  expect_equal(gg$grid[gg$reindex], g)
  
  g <- cbind(c(2, 2, NA, 1, NA), c(NA, NA, 3, 4, 4))
  gg <- .compress_grid(g)
  expect_equal(gg$grid[gg$reindex, , drop = FALSE], g)
  
  g <- data.frame(g)
  gg <- .compress_grid(g)
  res <- gg$grid[gg$reindex, , drop = FALSE]
  rownames(res) <- 1:5
  expect_equal(res, g)
})

test_that(".compress_X() works with missing values", {
  # Note that b is not used after compression
  
  # data.frame
  X <- data.frame(a = c(NA, NA, NA, 1, 1), b = 1:5)
  out_df <- data.frame(a = c(NA, 1), b = c(1, 4), row.names = c(1L, 4L))
  expect_warning(out <- .compress_X(X, v = "b"))
  expect_equal(out$X, out_df)
  expect_equal(out$w, c(3, 2))
  
  # Matrix
  X <- cbind(a = c(NA, NA, NA, 1, 1), b = 1:5)
  out_m <- cbind(a = c(NA, 1), b = c(1, 4))
  expect_warning(out <- .compress_X(X, v = "b"))
  expect_equal(out$X, out_m)
  expect_equal(out$w, c(3, 2))
})

test_that("pd_raw() works with missings (all compressions on)", {
  X <- cbind(a = c(NA, NA, NA, 1, 1), b = 1:5)
  out <- pd_raw(1, v = "a", X = X, pred_fun = function(m, x) x[, "b"], grid = c(NA, 1))
  expect_equal(drop(out), rep(mean(X[, "b"]), times = 2L))
  
  expect_warning(
    out <- pd_raw(1, v = "b", X = X, pred_fun = function(m, x) x[, "b"], grid = 1:5)
  )
  expect_equal(drop(out), 1:5)
})

# Other utils

test_that("qcut() works with missings", {
  expect_true(is.na(hstats:::qcut(c(NA, 1:9), m = 2)[1L]))
})
