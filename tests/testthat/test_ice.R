fit1 <- lm(Sepal.Length ~ . + Petal.Width * Species, data = iris)
fit2 <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
iris2 <- iris[c(1:10, 50:60, 100:110), ]

test_that("ice() returns same as partial_dep() for one row", {
  ic <- ice(fit1, v = "Species", X = iris[1L, ])$data[2:3]
  pd <- partial_dep(fit1, v = "Species", X = iris[1L, ])$data
  expect_equal(ic, pd)
})

test_that("print method does not give an error", {
  ic <- ice(fit1, v = "Species", X = iris)
  capture_output(expect_no_error(print(ic)))
})

test_that("ice() returns the same values as ice_raw()", {
  g <- rev(univariate_grid(iris$Species))
  ic1 <- unname(ice_raw(fit1, v = "Species", X = iris2, grid = g))
  ic2 <- ice(fit1, v = "Species", X = iris2, grid = g)$data$y
  expect_equal(ic1, ic2)
  
  ic1 <- ice_raw(fit2, v = "Species", X = iris2, grid = g)
  rownames(ic1) <- NULL
  ic2 <- ice(fit2, v = "Species", X = iris2, grid = g)$data[, colnames(ic1)]
  expect_equal(ic1, as.matrix(ic2))
})

test_that("ice() reacts on grid order", {
  g1 <- univariate_grid(iris$Species)
  g2 <- rev(g1)
  
  ic1 <- ice(fit1, v = "Species", X = iris2[1L, ], grid = g1)$data
  ic2 <- ice(fit1, v = "Species", X = iris2[1L, ], grid = g2)$data

  rownames(ic1) <- 1:3
  rownames(ic2) <- 3:1
  expect_equal(ic1, ic2[3:1, ])
  
  ic1 <- ice(fit2, v = "Species", X = iris2[1L, ], grid = g1)$data
  ic2 <- ice(fit2, v = "Species", X = iris2[1L, ], grid = g2)$data
  
  rownames(ic1) <- 1:3
  rownames(ic2) <- 3:1
  expect_equal(ic1, ic2[3:1, ])
})

test_that("ice() does subsampling", {
  set.seed(1L)
  ic1 <- ice(fit1, v = "Sepal.Width", X = iris, n_max = 10L, BY = 1:150)

  set.seed(2L)
  ic2 <- ice(fit1, v = "Sepal.Width", X = iris, n_max = 10L, BY = 1:150)
  
  expect_false(identical(ic1, ic2))
})

test_that("ice() reacts on grid strategy", {
  ic1 <- ice(
    fit1, v = "Sepal.Width", X = iris2, strategy = "uniform", grid_size = 5L
  )
  ic2 <- ice(
    fit1, v = "Sepal.Width", X = iris2, strategy = "quantile", grid_size = 5L
  )
  expect_false(identical(ic1, ic2))
})

test_that("ice() reacts on grid size", {
  ic1 <- ice(
    fit1, v = "Sepal.Width", X = iris2, strategy = "q", grid_size = 5L
  )
  ic2 <- ice(
    fit1, v = "Sepal.Width", X = iris2, strategy = "q", grid_size = 10L
  )
  expect_false(identical(ic1, ic2))
})

test_that("ice() reacts on grid", {
  g <- 1:4
  ic1 <- ice(fit1, v = "Sepal.Width", X = iris2, strategy = "q", grid = g)
  ic2 <- ice(fit1, v = "Sepal.Width", X = iris2, strategy = "q")
  expect_false(identical(ic1, ic2))
})

test_that("ice() reacts on trim", {
  ic1 <- ice(
    fit1, 
    v = "Sepal.Width", 
    X = iris2, 
    plot = FALSE, 
    strategy = "q", 
    trim = c(0.2, 0.8),
    grid_size = 5L
  )
  ic2 <- ice(
    fit1, 
    v = "Sepal.Width", 
    X = iris2, 
    plot = FALSE, 
    strategy = "q",
    trim = 0:1, 
    grid_size = 5L,
  )
  expect_false(identical(ic1, ic2))
})

test_that("ice() works with vector BY or variable name BY", {
  ic1 <- ice(fit1, v = "Sepal.Width", X = iris2, BY = "Species")
  ic2 <- ice(fit1, v = "Sepal.Width", X = iris2, BY = iris2$Species)
  colnames(ic2$data)[4L] <- "Species"
  expect_equal(ic1$data, ic2$data)
  expect_error(ice(fit1, v = "Sepal.Width", X = iris2, BY = iris$Species[1:10]))
})

test_that("ice() works with two BY", {
  b <- c("Petal.Width", "Species")
  ic1 <- ice(fit1, v = "Sepal.Width", X = iris2, BY = b)
  ic2 <- ice(fit1, v = "Sepal.Width", X = iris2, BY = iris2[b])
  colnames(ic2$data)[4:5] <- b
  expect_equal(ic1$data, ic2$data)
  expect_error(ice(fit1, v = "Sepal.Width", X = iris2, BY = iris[1:10, b]))
})

test_that("ice() works on matrices and dfs", {
  X <- data.matrix(iris[1:4])
  fitdf <- lm(Sepal.Length ~ Sepal.Width + Petal.Width + Petal.Length, data = iris)
  fitm <- lm(X[, 1] ~ Sepal.Width + Petal.Width + Petal.Length, data = as.data.frame(X))
  ic1 <- ice(fitdf, v = "Sepal.Width", X = iris2)
  ic2 <- ice(
    fitm, 
    v = "Sepal.Width", 
    X = X[c(1:10, 50:60, 100:110), ], 
    pred_fun = function(m, x) predict(m, as.data.frame(x))
  )
  expect_equal(ic1, ic2)
})

# Some plots
test_that("Plots give 'ggplot' objects", {
  fit <- lm(Sepal.Length ~ . + Species * Petal.Length, data = iris)
  
  # One v, no by, univariate
  expect_s3_class(plot(ice(fit, v = "Species", X = iris2)), "ggplot")
  
  # Two v give error
  ic <- ice(fit, v = c("Species", "Petal.Width"), X = iris2)
  expect_error(plot(ic))
  
  # One v, one by, univariate
  expect_s3_class(
    plot(ice(fit, v = "Species", X = iris2, BY = "Petal.Width")), 
    "ggplot"
  )
  
  # Centered
  expect_s3_class(
    plot(ice(fit, v = "Species", X = iris2, BY = "Petal.Width"), center = TRUE), 
    "ggplot"
  )
  
  # One v, two by, univariate
  expect_s3_class(
    plot(ice(fit, v = "Petal.Length", X = iris2, BY = c("Petal.Width", "Species"))), 
    "ggplot"
  )
  
  # Now multioutput
  fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
  
  # One v, no by, multivariate
  expect_s3_class(
    plot(ice(fit, v = "Species", X = iris2)), 
    "ggplot"
  )
  expect_s3_class(
    plot(ice(fit, v = "Species", X = iris2), swap_dim = TRUE), 
    "ggplot"
  )
  
  # Same centered
  expect_s3_class(
    plot(ice(fit, v = "Species", X = iris2), center = TRUE), 
    "ggplot"
  )
  expect_s3_class(
    plot(ice(fit, v = "Species", X = iris2), center = TRUE, swap_dim = TRUE), 
    "ggplot"
  )
  
  # One v, one by, multivariate
  expect_s3_class(
    plot(ice(fit, v = "Species", X = iris2, BY = "Petal.Width")), 
    "ggplot"
  )
  
  # One v, two by, multivariate -> error
  ic <- ice(fit, v = "Petal.Length", X = iris2, BY = c("Petal.Width", "Species"))
  expect_error(plot(ic, facet_scales = "fixed"))
})

# Some tests with missing values
X <- data.frame(x1 = 1:6, x2 = c(NA, 1, 2, 1, 1, 3), x3 = factor(c("A", NA, NA, "B", "A", "A")))
y <- 1:6
pf <- function(fit, x) x$x1
fit <- "a model"

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
