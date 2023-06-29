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
