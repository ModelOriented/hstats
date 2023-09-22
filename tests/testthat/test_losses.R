test_that("losses are identical to stats package for specific case", {
  y <- 1:10
  p <- y^2
  expect_equal(loss_squared_error(y, p), gaussian()$dev.resid(y, p, 1))
  expect_equal(loss_gamma(y, p), Gamma()$dev.resid(y, p, 1))
  
  y <- c(0, 0, 0, 1, 1, 1)
  p <- c(0, 1, 0.1, 1, 0, 0.9)
  expect_equal(loss_poisson(y, p), poisson()$dev.resid(y, p, 1))
  expect_equal(loss_logloss(y, p), binomial()$dev.resid(y, p, 1) / 2)
})

test_that("losses are identical to stats package for specific case (multivariate)", {
  y <- replicate(2, 1:10)
  p <- y^2
  expect_equal(loss_squared_error(y, p), gaussian()$dev.resid(y, p, 1))
  expect_equal(loss_gamma(y, p), Gamma()$dev.resid(y, p, 1))
  
  y <- replicate(2, c(0, 0, 0, 1, 1, 1))
  p <- replicate(2, c(0, 1, 0.1, 1, 0, 0.9))
  expect_equal(loss_poisson(y, p), poisson()$dev.resid(y, p, 1))
  expect_equal(loss_logloss(y, p), binomial()$dev.resid(y, p, 1) / 2)
})

test_that("loss_absolute_error() works for specific case", {
  y <- 1:10
  p <- y^2
  expect_equal(loss_absolute_error(y, p), abs(y - p))
})

test_that("loss_absolute_error() works for specific case (multivariate)", {
  y <- replicate(2, 1:10)
  p <- y^2
  expect_equal(loss_absolute_error(y, p), abs(y - p))
})

test_that("loss_classification_error() works for specific case", {
  y <- iris$Species
  p <- rev(iris$Species)
  expect_equal(loss_classification_error(y, p), 0 + (y != p))
})

test_that("loss_classification_error() works for specific case (multivariate case)", {
  y <- replicate(2, iris$Species)
  p <- y[nrow(y):1, ]
  expect_equal(loss_classification_error(y, p), 0 + (y != p))
})

test_that("Some errors are thrown", {
  expect_error(loss_poisson(-1:1, 1:2))
  expect_error(loss_poisson(0:1, -1:0))
  expect_error(loss_gamma(0:1, 1:2))
  expect_error(loss_gamma(1:2, -1:0))
  expect_error(loss_logloss(-1:0, 0:1))
  expect_error(loss_logloss(0:1, -1:0))
  expect_error(loss_mlogloss(cbind(-1:0), 0:1))
  expect_error(loss_mlogloss(0:1, cbind(-1:0)))
  
  expect_error(check_dim(cbind(1:3), cbind(1:3, 1:3)))
})

test_that("xlogy works (univariate and multivariate)", {
  expect_equal(xlogy(1:3, 1:3), (1:3) * log(1:3))
  expect_equal(xlogy(0:2, 0:2), c(0, 0, 2 * log(2)))
  
  x <- cbind(c(0,   0, 4), c( 0, 1, 2))
  y <- cbind(c(100, 0, 0), c(10, 1, 2))
  expected <- cbind(c(0, 0, -Inf), c(0, 0, 2 * log(2)))
  expect_equal(xlogy(x, y), expected)
})

test_that("get_loss_fun() works", {
  expect_error(get_loss_fun("no_loss"))
  expect_equal(get_loss_fun("poisson"), loss_poisson)
})
