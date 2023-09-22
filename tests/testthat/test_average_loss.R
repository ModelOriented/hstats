# Univariate model
fit <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
y <- iris$Sepal.Length

test_that("average_loss() works ungrouped for regression", {
  s <- average_loss(fit, X = iris, y = y)
  expect_equal(drop(s), mean((y - predict(fit, iris))^2))
  
  s <- average_loss(fit, X = iris, y = y, loss = "absolute_error")
  expect_equal(drop(s), mean(abs(y - predict(fit, iris))))
  
  s <- average_loss(fit, X = iris, y = y, loss = "poisson")
  expect_equal(drop(s), mean(poisson()$dev.resid(y, predict(fit, iris), 1)))
  
  s <- average_loss(fit, X = iris, y = y, loss = "gamma")
  expect_equal(drop(s), mean(Gamma()$dev.resid(y, predict(fit, iris), 1)))
  
  s <- average_loss(fit, X = iris, y = y, loss = loss_absolute_error)
  expect_equal(drop(s), mean(abs(y - predict(fit, iris))))
})

test_that("average_loss() works with groups for regression", {
  s <- average_loss(fit, X = iris, y = y, BY = iris$Species)
  xpect <- by((y - predict(fit, iris))^2, FUN = mean, INDICES = iris$Species)
  expect_equal(drop(s), c(xpect))
})

test_that("average_loss() works with weights for regression", {
  s1 <- average_loss(fit, X = iris, y = y)
  s2 <- average_loss(fit, X = iris, y = y, w = rep(2, times = 150))
  s3 <- average_loss(fit, X = iris, y = y, w = 1:150)
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
})

test_that("average_loss() works with weights and grouped for regression", {
  g <- iris$Species
  s1 <- average_loss(fit, X = iris, y = y, BY = g)
  s2 <- average_loss(fit, X = iris, y = y, w = rep(2, times = 150), BY = g)
  s3 <- average_loss(fit, X = iris, y = y, w = 1:150, BY = g)
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
})

test_that("average_loss() can work with non-numeric predictions", {
  pf <- function(m, X) rep("setosa", times = nrow(X))
  expect_warning(average_loss(1, X = iris, y = iris$Species, pred_fun = pf))
  expect_equal(
    c(average_loss(
      1, X = iris, y = iris$Species, pred_fun = pf, loss = "classification_error"
    )),
    2/3
  )
  expect_equal(
    c(average_loss(
      1, 
      X = iris, 
      y = iris$Species, 
      pred_fun = pf, 
      loss = "classification_error",
      BY = iris$Species
    )),
    c(0, 1, 1)
  )
})

#================================================
# Multivariate regression
#================================================

y <- as.matrix(iris[1:2])
fit <- lm(y ~ Petal.Length + Species, data = iris)

test_that("average_loss() works ungrouped (multi regression)", {
  s <- average_loss(fit, X = iris, y = y)
  expect_equal(drop(s), colMeans((y - predict(fit, iris))^2))
  
  s <- average_loss(fit, X = iris, y = y, loss = "absolute_error")
  expect_equal(drop(s), colMeans(abs(y - predict(fit, iris))))
  
  s <- average_loss(fit, X = iris, y = y, loss = loss_absolute_error)
  expect_equal(drop(s), colMeans(abs(y - predict(fit, iris))))
  
  s <- average_loss(fit, X = iris, y = y, loss = "poisson")
  expect_equal(drop(s), colMeans(poisson()$dev.resid(y, predict(fit, iris), 1)))
  
  s <- average_loss(fit, X = iris, y = y, loss = "gamma")
  expect_equal(drop(s), colMeans(Gamma()$dev.resid(y, predict(fit, iris), 1)))
})

test_that("average_loss() works with groups (multi regression)", {
  s <- average_loss(fit, X = iris, y = y, BY = iris$Species)
  xpect <- by((y - predict(fit, iris))^2, FUN = colMeans, INDICES = iris$Species)
  expect_equal(s, do.call(rbind, xpect))
})

test_that("average_loss() works with weights (multi regression)", {
  s1 <- average_loss(fit, X = iris, y = y)
  s2 <- average_loss(fit, X = iris, y = y, w = rep(2, times = 150))
  s3 <- average_loss(fit, X = iris, y = y, w = 1:150)
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
})

test_that("average_loss() works with weights and grouped (multi regression)", {
  g <- iris$Species
  s1 <- average_loss(fit, X = iris, y = y, BY = g)
  s2 <- average_loss(fit, X = iris, y = y, w = rep(2, times = 150), BY = g)
  s3 <- average_loss(fit, X = iris, y = y, w = 1:150, BY = g)
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
})

#================================================
# Classification (probabilistic)
#================================================

y <- cbind(iris[, 1L] > 5.8, iris[, 2L] > 3) * 1
fit <- lm(y ~ Petal.Length + Species, data = iris)
pf <- function(m, X) {
  out <- predict(m, X)
  out[out<0] <- 0
  out[out>1] <- 1
  setNames(out, c("V1", "V2"))
}

test_that("average_loss() works ungrouped (multivariate binary)", {
  s <- average_loss(fit, X = iris, y = y, loss = "logloss", pred_fun = pf)
  expect_equal(drop(s), colMeans(binomial()$dev.resid(y, pf(fit, iris), 1)) / 2)
})

test_that("average_loss() works with groups (multivariate binary)", {
  s <- average_loss(fit, X = iris, y = y, loss = "logloss", 
                    BY = iris$Species, pred_fun = pf)
  xpect <- by(binomial()$dev.resid(y, pf(fit, iris), 1) / 2, 
    FUN = colMeans, INDICES = iris$Species)
  expect_equal(s, do.call(rbind, xpect))
})

test_that("average_loss() works with weights (multivariate binary)", {
  s1 <- average_loss(fit, X = iris, y = y, 
                     pred_fun = pf, loss = "logloss")
  s2 <- average_loss(fit, X = iris, y = y, w = rep(2, times = 150), 
                     pred_fun = pf, loss = "logloss")
  s3 <- average_loss(fit, X = iris, y = y, w = 1:150, 
                     pred_fun = pf, loss = "logloss")
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
})

test_that("average_loss() works with weights and grouped (multi)", {
  g <- iris$Species
  s1 <- average_loss(fit, X = iris, y = y, BY = g, 
                     pred_fun = pf, loss = "logloss")
  s2 <- average_loss(fit, X = iris, y = y, w = rep(2, times = 150), BY = g,
                     pred_fun = pf, loss = "logloss")
  s3 <- average_loss(fit, X = iris, y = y, w = 1:150, BY = g,
                     pred_fun = pf, loss = "logloss")
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
})

test_that("mlogloss works with either matrix y or vector y", {
  pred_fun <- function(m, X) cbind(1 - (0.1 + 0.7 * (X == 1)), 0.1 + 0.7 * (X == 1))
  X <- cbind(z = c(1, 0, 0, 1, 1))
  y <- c("B", "A", "B", "B", "B")
  Y <- model.matrix(~ y + 0)
  s1 <- average_loss(NULL, X = X, y = Y, loss = "mlogloss", pred_fun = pred_fun)
  s2 <- average_loss(NULL, X = X, y = y, loss = "mlogloss", pred_fun = pred_fun)
  expect_equal(s1, s2)
})

