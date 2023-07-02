fit1 <- lm(Sepal.Length ~ . + Petal.Width * Species, data = iris)
fit2 <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
iris2 <- iris[c(1:10, 50:60, 100:110), ]

test_that("ice() returns same as partial_dep() for one row", {
  ic <- ice(fit1, v = "Species", X = iris[1L, ])$ice_curves[2:3]
  pd <- partial_dep(fit1, v = "Species", X = iris[1L, ])$pd
  expect_equal(ic, pd)
})

test_that("ice() returns the same values as ice_raw()", {
  g <- rev(univariate_grid(iris$Species))
  ic1 <- c(ice_raw(fit1, v = "Species", X = iris2, grid = g))
  ic2 <- ice(fit1, v = "Species", X = iris2, grid = g)$ice_curves$y
  expect_equal(ic1, ic2)
  
  ic1 <- ice_raw(fit2, v = "Species", X = iris2, grid = g)
  rownames(ic1) <- NULL
  ic2 <- ice(fit2, v = "Species", X = iris2, grid = g)$ice_curves[, colnames(ic1)]
  expect_equal(ic1, as.matrix(ic2))
})

test_that("ice() reacts on grid order", {
  g1 <- univariate_grid(iris$Species)
  g2 <- rev(g1)
  
  ic1 <- ice(fit1, v = "Species", X = iris2[1L, ], grid = g1)$ice_curves
  ic2 <- ice(fit1, v = "Species", X = iris2[1L, ], grid = g2)$ice_curves

  rownames(ic1) <- 1:3
  rownames(ic2) <- 3:1
  expect_equal(ic1, ic2[3:1, ])
  
  ic1 <- ice(fit2, v = "Species", X = iris2[1L, ], grid = g1)$ice_curves
  ic2 <- ice(fit2, v = "Species", X = iris2[1L, ], grid = g2)$ice_curves
  
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
    fit1, v = "Sepal.Width", X = iris2, strategy = "uni", grid_size = 5L
  )
  ic2 <- ice(
    fit1, v = "Sepal.Width", X = iris2, strategy = "quant", grid_size = 5L
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
  colnames(ic2$ice_curves)[4L] <- "Species"
  expect_equal(ic1$ice_curves, ic2$ice_curves)
  expect_error(ice(fit1, v = "Sepal.Width", X = iris2, BY = iris$Species[1:10]))
})

test_that("ice() works with two BY", {
  b <- c("Petal.Width", "Species")
  ic1 <- ice(fit1, v = "Sepal.Width", X = iris2, BY = b)
  ic2 <- ice(fit1, v = "Sepal.Width", X = iris2, BY = iris2[b])
  colnames(ic2$ice_curves)[4:5] <- b
  expect_equal(ic1$ice_curves, ic2$ice_curves)
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
  
  # One v, one by, univariate
  expect_s3_class(
    plot(ice(fit, v = "Species", X = iris2, BY = "Petal.Width")), 
    "ggplot"
  )
  
  # One v, two by, univariate
  expect_s3_class(
    plot(ice(fit, v = "Petal.Length", X = iris2, BY = c("Petal.Width", "Species"))), 
    "ggplot"
  )
  
  # NOW multioutput
  fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
  
  # One v, no by, multivariate
  expect_s3_class(
    plot(ice(fit, v = "Species", X = iris2), color = "red"), 
    "ggplot"
  )
  
  # One v, one by, multivariate
  expect_s3_class(
    plot(
      ice(fit, v = "Species", X = iris2, BY = "Petal.Width"), 
      facet_scales = "fixed"
    ), 
    "ggplot"
  )
  
  # One v, two by, multivariate -> error
  ic <- ice(fit, v = "Petal.Length", X = iris2, BY = c("Petal.Width", "Species"))
  expect_error(plot(ic, facet_scales = "fixed"))
})
