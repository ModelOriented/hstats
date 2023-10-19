pfun <- function(m, x) cbind(pred = x[, "a"] / 2)
X <- data.frame(a = 1:2, b = 10:11)
g <- 2:1

test_that("pd_raw() works for simple example", {
  pd <- pd_raw(1, v = "a", X = X, grid = g, pred_fun = pfun)
  expect_equal(pd, cbind(pred = g / 2))
})

test_that("pd_raw() works on simple example with and without grid compression", {
  g <- c(3, 3, 1, 2)
  pd1 <- pd_raw(
    1, v = "a", X = X, grid = g, pred_fun = pfun, compress_grid = FALSE
  )
  pd2 <- pd_raw(
    1, v = "a", X = X, grid = g, pred_fun = pfun, compress_grid = TRUE
  )
  expect_equal(pd1, cbind(pred = g / 2))
  expect_equal(pd2, cbind(pred = g / 2))
})

test_that("pd_raw() works on simple example with and without X compression", {
  g <- 2:1
  X2 <- data.frame(a = 1:4, b = c(1, 1, 1, 2))
  pd1 <- pd_raw(
    1, v = "a", X = X2, grid = g, pred_fun = pfun, compress_X = FALSE
  )
  pd2 <- pd_raw(
    1, v = "a", X = X2, grid = g, pred_fun = pfun, compress_X = TRUE
  )
  expect_equal(pd1, cbind(pred = g / 2))
  expect_equal(pd2, cbind(pred = g / 2))
})

fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)

test_that("pd_raw() gives same answer on example as iml 0.11.1", {
  # library(iml)
  # mod <- Predictor$new(fit, data = iris)
  # FeatureEffect$new(mod, feature = "Species", method = "pdp")$results
  # FeatureEffect$new(mod, feature = "Sepal.Width", method = "pdp", grid.points = 2:4)$results

  iml_species <- c(6.847179, 5.737053, 5.260083)
  raw_species <- c(pd_raw(fit, v = "Species", X = iris, grid = unique(iris$Species)))
  expect_equal(iml_species, raw_species, tolerance = 0.001)

  iml_sw <- c(5.309279, 5.814375, 6.319470)
  raw_sw <- c(pd_raw(fit, v = "Sepal.Width", X = iris, grid = 2:4))
  expect_equal(iml_sw, raw_sw, tolerance = 0.001)
})

test_that("pd_raw() works with case weights on non-trivial example", {
  w <- rep(2, times = 150)
  pd1 <- pd_raw(fit, v = "Species", X = iris, grid = unique(iris$Species))
  pd2 <- pd_raw(fit, v = "Species", X = iris, grid = unique(iris$Species), w = w)
  expect_equal(pd1, pd2)
  
  pd2 <- pd_raw(fit, v = "Species", X = iris, grid = unique(iris$Species), w = 1:150)
  expect_false(identical(pd1, pd2))
})

test_that("pd_raw() respects ... argument for predictions", {
  fit2 <- glm(Sepal.Length ~ . + Petal.Width:Species, data = iris, family = Gamma())
  
  pd1 <- pd_raw(fit2, v = "Species", X = iris, grid = unique(iris$Species))
  pd2 <- pd_raw(
    fit2, v = "Species", X = iris, grid = unique(iris$Species), type = "response"
  )
  expect_false(identical(pd1, pd2))
})

test_that("pd_raw() can operate on multivariate grid", {
  g <- multivariate_grid(iris[4:5])
  pd1 <- pd_raw(fit, v = colnames(g), X = iris, grid = g)
  
  expect_equal(nrow(pd1), nrow(g))
})

test_that("pd_raw() also works for multioutput situations", {
  fit3 <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
  pd <- pd_raw(fit3, v = "Petal.Width", X = iris, grid = 2:3)
  expect_equal(dim(pd), c(2L, 2L))
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

# Now, partial_dep()
fit1 <- lm(Sepal.Length ~ . + Petal.Width * Species, data = iris)
fit2 <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)

test_that("print method does not give an error", {
  pd <- partial_dep(fit1, v = "Species", X = iris)
  capture_output(expect_no_error(print(pd)))
})

test_that("partial_dep() returns the same values as pd_raw()", {
  g <- rev(univariate_grid(iris$Species))
  pd1 <- pd_raw(fit1, v = "Species", X = iris, grid = g)
  pd2 <- partial_dep(fit1, v = "Species", X = iris, grid = g)
  expect_equal(cbind.data.frame(Species = g, y = pd1), pd2$data)
  
  pd1 <- pd_raw(fit2, v = "Species", X = iris, grid = g)
  pd2 <- partial_dep(fit2, v = "Species", X = iris, grid = g)
  expect_equal(cbind.data.frame(Species = g, pd1), pd2$data)
})

test_that("partial_dep() reacts on grid order", {
  g1 <- univariate_grid(iris$Species)
  g2 <- rev(g1)
  
  pd1 <- partial_dep(fit1, v = "Species", X = iris, grid = g1)$data
  pd2 <- partial_dep(fit1, v = "Species", X = iris, grid = g2)$data

  rownames(pd2) <- 3:1
  expect_equal(pd1, pd2[3:1, ])
  
  pd1 <- partial_dep(fit2, v = "Species", X = iris, grid = g1)$data
  pd2 <- partial_dep(fit2, v = "Species", X = iris, grid = g2)$data
  
  rownames(pd2) <- 3:1
  expect_equal(pd1, pd2[3:1, ])
})

test_that("partial_dep() with BY is same as stratified application", {
  pd1 <- partial_dep(fit1, v = "Sepal.Width", X = iris, BY = "Species")
  g <- unique(pd1$data$Sepal.Width)
  ir <- split(iris, iris$Species)
  pd2 <- rbind(
    partial_dep(fit1, v = "Sepal.Width", X = ir[[1L]], grid = g)$data,
    partial_dep(fit1, v = "Sepal.Width", X = ir[[2L]], grid = g)$data,
    partial_dep(fit1, v = "Sepal.Width", X = ir[[3L]], grid = g)$data
  )
  pd2 <- data.frame(Species = rep(unique(iris$Species), each = length(g)), pd2)
  expect_equal(pd1$data, pd2)
  
  # Multioutput
  pd1 <- partial_dep(fit2, v = "Petal.Width", X = iris, BY = "Species")$data
  g <- unique(pd1$Petal.Width)
  ir <- split(iris, iris$Species)
  pd2 <- rbind(
    partial_dep(fit2, v = "Petal.Width", X = ir[[1L]], grid = g)$data,
    partial_dep(fit2, v = "Petal.Width", X = ir[[2L]], grid = g)$data,
    partial_dep(fit2, v = "Petal.Width", X = ir[[3L]], grid = g)$data
  )
  pd2 <- data.frame(Species = rep(unique(iris$Species), each = length(g)), pd2)
  expect_equal(pd1, pd2)
})

test_that("partial_dep() does subsampling", {
  set.seed(1L)
  pd1 <- partial_dep(
    fit1, v = "Sepal.Width", X = iris, n_max = 10L, w = iris$Petal.Width
  )

  set.seed(2L)
  pd2 <- partial_dep(fit1, v = "Sepal.Width", X = iris, n_max = 10L, w = "Petal.Width")
  
  expect_false(identical(pd1, pd2))
})

test_that("partial_dep() reacts on grid strategy", {
  pd1 <- partial_dep(
    fit1, v = "Sepal.Width", X = iris, strategy = "uniform", grid_size = 5L
  )
  pd2 <- partial_dep(
    fit1, v = "Sepal.Width", X = iris, strategy = "quantile", grid_size = 5L
  )
  expect_false(identical(pd1, pd2))
})

test_that("partial_dep() reacts on grid size", {
  pd1 <- partial_dep(
    fit1, v = "Sepal.Width", X = iris, strategy = "q", grid_size = 5L
  )
  pd2 <- partial_dep(
    fit1, v = "Sepal.Width", X = iris, strategy = "q", grid_size = 10L
  )
  expect_false(identical(pd1, pd2))
  
  pd1 <- partial_dep(
    fit1, v = "Sepal.Width", X = iris, strategy = "u", grid_size = 5L
  )
  pd2 <- partial_dep(
    fit1, v = "Sepal.Width", X = iris, strategy = "u", grid_size = 10L
  )
  expect_false(identical(pd1, pd2))
})

test_that("partial_dep() reacts on grid", {
  g <- 1:4
  pd1 <- partial_dep(fit1, v = "Sepal.Width", X = iris, grid = g)
  pd2 <- partial_dep(fit1, v = "Sepal.Width", X = iris, strategy = "q")
  expect_false(identical(pd1, pd2))
})

test_that("partial_dep() reacts on trim", {
  pd1 <- partial_dep(
    fit1, 
    v = "Sepal.Width", 
    X = iris, 
    plot = FALSE, 
    strategy = "q", 
    trim = c(0.2, 0.8),
    grid_size = 5L
  )
  pd2 <- partial_dep(
    fit1, 
    v = "Sepal.Width", 
    X = iris, 
    plot = FALSE, 
    strategy = "q",
    trim = 0:1, 
    grid_size = 5L,
  )
  expect_false(identical(pd1, pd2))
  
  pd1 <- partial_dep(
    fit1, v = "Sepal.Width", X = iris, trim = c(0.2, 0.8), grid_size = 5L
  )
  pd2 <- partial_dep(
    fit1, v = "Sepal.Width", X = iris, trim = 0:1, grid_size = 5L,
  )
  expect_false(identical(pd1, pd2))
})

test_that("partial_dep() does not react on constant weights", {
  pd1 <- partial_dep(fit1, v = "Sepal.Width", X = iris)
  pd2 <- partial_dep(fit1, v = "Sepal.Width", X = iris, w = rep(2, times = 150L))
  expect_equal(pd1, pd2)
})

test_that("partial_dep() reacts on non-constant weights", {
  pd1 <- partial_dep(fit1, v = "Sepal.Width", X = iris)
  pd2 <- partial_dep(fit1, v = "Sepal.Width", X = iris, w = iris$Petal.Width)
  pd3 <- partial_dep(fit1, v = "Sepal.Width", X = iris, w = "Petal.Width")
  expect_false(identical(pd1, pd2))
  expect_equal(pd2, pd3)
})

test_that("partial_dep() works with vector BY or variable name BY", {
  pd1 <- partial_dep(fit1, v = "Sepal.Width", X = iris, BY = "Species")
  pd2 <- partial_dep(fit1, v = "Sepal.Width", X = iris, BY = iris$Species)
  colnames(pd2$data)[1L] <- "Species"
  expect_equal(pd1$data, pd2$data)
  expect_error(partial_dep(fit1, v = "Sepal.Width", X = iris, BY = iris$Species[1:10]))
})

test_that("partial_dep() gives same answer on example as iml 0.11.1", {
  # library(iml)
  # mod <- Predictor$new(fit, data = iris)
  # FeatureEffect$new(mod, feature = "Species", method = "pdp")$results
  # FeatureEffect$new(mod, feature = "Sepal.Width", method = "pdp", grid.points = 2:4)$results
  
  iml_species <- c(6.847179, 5.737053, 5.260083)
  pd1 <- partial_dep(fit, v = "Species", X = iris)$data
  expect_equal(iml_species, pd1$y, tolerance = 0.001)
  
  iml_sw <- c(5.309279, 5.814375, 6.319470)
  pd2 <- partial_dep(fit, v = "Sepal.Width", X = iris, grid = 2:4)$data
  expect_equal(iml_sw, pd2$y, tolerance = 0.001)
})

test_that("partial_dep() works on matrices and dfs", {
  X <- data.matrix(iris[1:4])
  fitdf <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length, data = iris)
  fitm <- lm(X[, 1] ~ Sepal.Width + Petal.Width + Petal.Length, data = as.data.frame(X))
  pd1 <- partial_dep(fitdf, v = "Sepal.Width", X = iris)
  pd2 <- partial_dep(
    fitm, 
    v = "Sepal.Width", 
    X = X, 
    pred_fun = function(m, x) predict(m, as.data.frame(x))
  )
  expect_equal(pd1, pd2)
})

# Some plots
test_that("Plots give 'ggplot' objects", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  
  # One v, no by, univariate
  expect_s3_class(plot(partial_dep(fit, v = "Species", X = iris)), "ggplot")
  
  # One v, with by, univariate
  pd <- partial_dep(fit, v = "Species", X = iris, BY = "Petal.Width")
  expect_s3_class(plot(pd), "ggplot")
  expect_s3_class(plot(pd, swap_dim = TRUE), "ggplot")
  
  # Two v, no by, univariate
  v <- c("Petal.Length", "Petal.Width")
  pd <- partial_dep(fit, v = v, X = iris)
  expect_s3_class(plot(pd), "ggplot")
  
  # Two v, no by, univariate, prespecified grid
  g <- unique(iris[v])
  pd <- partial_dep(fit, v = v, X = iris, grid = g)
  expect_s3_class(plot(pd, d2_geom = "point"), "ggplot")
  
  # Two v, with by, univariate
  pd <- partial_dep(fit, v = v, X = iris, BY = "Species")
  expect_s3_class(plot(pd), "ggplot")
  
  # Two v, with by, univariate, prespecified grid
  pd <- partial_dep(fit, v = v, X = iris, BY = "Species", grid = g)
  expect_s3_class(plot(pd, d2_geom = "point"), "ggplot")
  
  # Three v gives error
  pd <- partial_dep(fit, v = c(v, "Species"), X = iris)
  expect_error(plot(pd))
  
  # Now multioutput
  fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
  
  # One v, no by, multivariate
  pd <- partial_dep(fit, v = "Species", X = iris)
  expect_s3_class(plot(pd), "ggplot")
  expect_s3_class(plot(pd, color = "red", swap_dim = TRUE), "ggplot")
  
  # One v, with by, multivariate
  pd <- partial_dep(fit, v = "Species", X = iris, BY = "Petal.Width")
  expect_s3_class(plot(pd, facet_scales = "free_y"), "ggplot")
  expect_s3_class(plot(pd, swap_dim = TRUE), "ggplot")
  
  # Two v, no by, multivariate
  pd <- partial_dep(fit, v = v, X = iris)
  expect_s3_class(plot(pd, rotate_x = TRUE), "ggplot")
  
  # Two v, no by, multivariate, prespecified grid
  pd <- partial_dep(fit, v = v, X = iris, grid = g)
  expect_s3_class(plot(pd, d2_geom = "point", alpha = 0.5), "ggplot")
  
  # Two v, with by, multivariate gives error
  pd <- partial_dep(fit, v = v, X = iris, BY = "Petal.Width")
  expect_error(plot(pd))
})

# Some checks with missing values
X <- data.frame(x1 = 1:6, x2 = c(NA, 1, 2, 1, 1, 3), x3 = factor(c("A", NA, NA, "B", "A", "A")))
y <- 1:6
pf <- function(fit, x) x$x1
fit <- "a model"

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
