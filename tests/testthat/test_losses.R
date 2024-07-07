test_that("losses are identical to stats package for specific case", {
  y <- 1:10
  p <- y^2
  expect_equal(loss_squared_error(y, p), gaussian()$dev.resid(y, p, 1))
  expect_equal(loss_gamma(y, p), Gamma()$dev.resid(y, p, 1))
  
  y <- c(0, 0, 0, 1, 1, 1)
  p <- c(0, 1, 0.1, 1, 0, 0.9)
  expect_equal(loss_poisson(y, p), poisson()$dev.resid(y, p, 1))
  expect_equal(loss_logloss(y, p), binomial()$dev.resid(y, p, 1) / 2)
  
  # Single values
  y <- 3
  p <- y^2
  expect_equal(loss_squared_error(y, p), gaussian()$dev.resid(y, p, 1))
  expect_equal(loss_gamma(y, p), Gamma()$dev.resid(y, p, 1))
  
  y <- 0
  p <- 0.1
  expect_equal(loss_poisson(y, p), poisson()$dev.resid(y, p, 1))
  expect_equal(loss_logloss(y, p), binomial()$dev.resid(y, p, 1) / 2)
})

test_that("losses are identical to stats package for specific case (multivariate)", {
  y <- 1:10
  y2 <- replicate(2, y)
  p <- y2^2
  
  expect_equal(loss_squared_error(y2, p), gaussian()$dev.resid(y2, p, 1))
  expect_equal(loss_squared_error(y, p), gaussian()$dev.resid(y2, p, 1))
  
  expect_equal(loss_gamma(y2, p), Gamma()$dev.resid(y2, p, 1))
  expect_equal(loss_gamma(y, p), Gamma()$dev.resid(y2, p, 1))
  
  y <- c(0, 0, 0, 1, 1, 1)
  y2 <- replicate(2, y)
  p <- replicate(2, c(0, 1, 0.1, 1, 0, 0.9))
  
  expect_equal(loss_poisson(y2, p), poisson()$dev.resid(y2, p, 1))
  expect_equal(loss_poisson(y, p), poisson()$dev.resid(y2, p, 1))
  
  expect_equal(loss_logloss(y2, p), binomial()$dev.resid(y2, p, 1) / 2)
  expect_equal(loss_logloss(y, p), binomial()$dev.resid(y2, p, 1) / 2)
  
  # Single input
  y <- 3
  y2 <- rbind(replicate(2, y))
  p <- y2^2
  
  expect_equal(loss_squared_error(y2, p), gaussian()$dev.resid(y2, p, 1))
  expect_equal(loss_squared_error(y, p), gaussian()$dev.resid(y2, p, 1))
  
  expect_equal(loss_gamma(y2, p), Gamma()$dev.resid(y2, p, 1))
  expect_equal(loss_gamma(y, p), Gamma()$dev.resid(y2, p, 1))
  
  y <- 0
  y2 <- cbind(y, y)
  colnames(y2) <- NULL
  p <- rbind(replicate(2, 0.1))
  
  expect_equal(loss_poisson(y2, p), poisson()$dev.resid(y2, p, 1))
  expect_equal(loss_poisson(y, p), poisson()$dev.resid(y2, p, 1))
  
  expect_equal(loss_logloss(y2, p), rbind(binomial()$dev.resid(y2, p, 1) / 2))
  expect_equal(loss_logloss(y, p), rbind(binomial()$dev.resid(y2, p, 1) / 2))
})

test_that("loss_absolute_error() works for specific case", {
  y <- 1:10
  p <- y^2
  expect_equal(loss_absolute_error(y, p), abs(y - p))
  
  # Single input
  y <- 3
  p <- y^2
  expect_equal(loss_absolute_error(y, p), abs(y - p))
})

test_that("loss_absolute_error() works for specific case (multivariate)", {
  y <- 1:10
  y2 <- replicate(2, y)
  p <- y2^2
  colnames(p) <- c("a", "b")
  expect_equal(loss_absolute_error(y2, p), abs(y2 - p))
  expect_equal(loss_absolute_error(y, p), abs(y2 - p))
  
  # Single input
  y <- 3
  y2 <- rbind(replicate(2, y))
  p <- y2^2
  colnames(p) <- c("a", "b")
  expect_equal(loss_absolute_error(y2, p), abs(y2 - p))
  expect_equal(loss_absolute_error(y, p), abs(y2 - p))
})

test_that("loss_mlogloss() is in line with loss_logloss() in binary case", {
  y <- c(0, 0, 0, 1, 1)
  pred <- c(0, 0.1, 0.2, 1, 0.9)
  y2 <- cbind(a = 1 - y, b = y)
  pred2 <- cbind(1 - pred, pred)
  expect_equal(loss_mlogloss(y2, pred2), loss_logloss(y, pred))
  expect_equal(loss_mlogloss(y, pred2), loss_logloss(y, pred))
})

test_that("loss_mlogloss() either understands matrix responses or factors", {
  y <- iris$Species
  Y <- model.matrix(~ Species + 0, data = iris)
  fit <- lm(Y ~ Sepal.Width, data = iris)
  pf <- function(m, X) {
    out <- predict(m, X)
    out[out < 0] <- 0
    out[out > 1] <- 1
    out
  }
  pred <- pf(fit, iris)
  expect_equal(loss_mlogloss(Y, pred), loss_mlogloss(y, pred))
})

test_that("Some errors are thrown", {
  expect_error(loss_poisson(-1:1, 1:2))
  expect_error(loss_poisson(0:1, -1:0))
  expect_error(loss_poisson(iris$Species, iris$Species))
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

