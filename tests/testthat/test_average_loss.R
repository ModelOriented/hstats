# Univariate model
fit <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
y <- iris$Sepal.Length

test_that("average_loss() works ungrouped for regression", {
  s <- average_loss(fit, X = iris, y = y)$M
  s2 <- average_loss(fit, X = iris, y = "Sepal.Length")$M
  expect_equal(drop(s), mean((y - predict(fit, iris))^2))
  expect_equal(s, s2)
  
  s <- average_loss(fit, X = iris, y = y, loss = "absolute_error")$M
  expect_equal(drop(s), mean(abs(y - predict(fit, iris))))
  
  s <- average_loss(fit, X = iris, y = y, loss = "poisson")$M
  expect_equal(drop(s), mean(poisson()$dev.resid(y, predict(fit, iris), 1)))
  
  s <- average_loss(fit, X = iris, y = y, loss = "gamma")$M
  expect_equal(drop(s), mean(Gamma()$dev.resid(y, predict(fit, iris), 1)))
  
  s <- average_loss(fit, X = iris, y = y, loss = loss_absolute_error)$M
  expect_equal(drop(s), mean(abs(y - predict(fit, iris))))
})

test_that("average_loss() works with groups for regression", {
  s <- average_loss(fit, X = iris, y = y, BY = iris$Species)$M
  s2 <- average_loss(fit, X = iris, y = "Sepal.Length", BY = "Species")$M
  
  xpect <- by((y - predict(fit, iris))^2, FUN = mean, INDICES = iris$Species)
  expect_equal(drop(s), c(xpect))
  expect_equal(s, s2)
  
  expect_equal(dim(average_loss(fit, X = iris, y = y, BY = "Sepal.Width")$M), c(4L, 1L))
  expect_equal(
    dim(average_loss(fit, X = iris, y = "Sepal.Width", BY = "Sepal.Width", by_size = 2L)$M), 
    c(2L, 1L)
  )
})

test_that("average_loss() works with weights for regression", {
  s1 <- average_loss(fit, X = iris, y = y)
  s2 <- average_loss(fit, X = iris, y = "Sepal.Length", w = rep(2, times = 150))
  s3 <- average_loss(fit, X = iris, y = y, w = "Petal.Width")
  s4 <- average_loss(fit, X = iris, y = "Sepal.Length", w = iris$Petal.Width)
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
  expect_equal(s3, s4)
  expect_error(average_loss(fit, X = iris, y = y, w = 1:4))
  expect_error(average_loss(fit, X = iris, y = y, w = "bad_column"))
})

test_that("average_loss() works with weights and grouped for regression", {
  g <- iris$Species
  s1 <- average_loss(fit, X = iris, y = y, BY = g)
  s2 <- average_loss(
    fit, X = iris, y = "Sepal.Length", w = rep(2, times = 150), BY = "Species"
  )
  s3 <- average_loss(fit, X = iris, y = "Sepal.Length", w = "Petal.Width", BY = g)
  s4 <- average_loss(fit, X = iris, y = y, w = iris$Petal.Width, BY = g)
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
  expect_equal(s3, s4)
})

test_that("agg_cols = TRUE has no effect", {
  s <- average_loss(fit, X = iris, y = y)
  s2 <- average_loss(fit, X = iris, y = y, agg_cols = TRUE)
  expect_equal(s, s2)
  
  s <- average_loss(fit, X = iris, y = y, BY = "Species")
  s2 <- average_loss(fit, X = iris, y = y, BY = "Species", agg_cols = TRUE)
  expect_equal(s, s2)
})

#================================================
# Multivariate regression
#================================================

y <- as.matrix(iris[1:2])
yy <- colnames(y)
fit <- lm(y ~ Petal.Length + Species, data = iris)

test_that("average_loss() works ungrouped (multi regression)", {
  s <- average_loss(fit, X = iris, y = y)$M
  expect_equal(drop(s), colMeans((y - predict(fit, iris))^2))
  s2 <- average_loss(fit, X = iris, y = yy)$M
  expect_equal(s, s2)
  
  s <- average_loss(fit, X = iris, y = y, loss = "absolute_error")$M
  expect_equal(drop(s), colMeans(abs(y - predict(fit, iris))))
  
  s <- average_loss(fit, X = iris, y = y, loss = loss_absolute_error)$M
  expect_equal(drop(s), colMeans(abs(y - predict(fit, iris))))
  
  s <- average_loss(fit, X = iris, y = y, loss = "poisson")$M
  expect_equal(drop(s), colMeans(poisson()$dev.resid(y, predict(fit, iris), 1)))
  
  s <- average_loss(fit, X = iris, y = yy, loss = "gamma")$M
  expect_equal(drop(s), colMeans(Gamma()$dev.resid(y, predict(fit, iris), 1)))
})

test_that("average_loss() works with groups (multi regression)", {
  s <- average_loss(fit, X = iris, y = y, BY = iris$Species)$M
  xpect <- by((y - predict(fit, iris))^2, FUN = colMeans, INDICES = iris$Species)
  expect_equal(s, do.call(rbind, xpect))
})

test_that("average_loss() works with weights (multi regression)", {
  s1 <- average_loss(fit, X = iris, y = y)
  s2 <- average_loss(fit, X = iris, y = yy, w = rep(2, times = 150))
  s3 <- average_loss(fit, X = iris, y = y, w = iris$Petal.Width)
  s4 <- average_loss(fit, X = iris, y = yy, w = "Petal.Width")
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
  expect_equal(s3, s4)
})

test_that("average_loss() works with weights and grouped (multi regression)", {
  g <- iris$Species
  s1 <- average_loss(fit, X = iris, y = y, BY = g)
  s2 <- average_loss(fit, X = iris, y = yy, w = rep(2, times = 150), BY = g)
  s3 <- average_loss(fit, X = iris, y = y, w = iris$Petal.Width, BY = g)
  s4 <- average_loss(fit, X = iris, y = yy, w = "Petal.Width", BY = g)
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
  expect_equal(s3, s4)
})

test_that("agg_cols = TRUE has an effect", {
  s <- average_loss(fit, X = iris, y = y)
  s2 <- average_loss(fit, X = iris, y = y, agg_cols = TRUE)
  expect_equal(rowSums(s$M), drop(s2$M))
 
  s <- average_loss(fit, X = iris, y = y, BY = "Species")
  s2 <- average_loss(fit, X = iris, y = y, agg_cols = TRUE, BY = "Species")
  expect_equal(rowSums(s$M), drop(s2$M))
})

test_that("Single output multiple models works without recycling y", {
  y <- iris$Sepal.Length
  Y <- cbind(f1 = y, f2 = y)
  fit1 <- lm(y ~ Petal.Length + Species, data = iris)
  fit2 <- lm(y ~ Petal.Width + Sepal.Width, data = iris)
  fit <- list(f1 = fit1, f2 = fit2)
  pf <- function(m, x) sapply(fit, FUN = predict, newdata = x)
  
  s <- average_loss(fit, X = iris, y = y, pred_fun = pf)$M
  expect_equal(drop(s), colMeans((y - pf(fit, iris))^2))
  
  s <- average_loss(fit, X = iris, y = y, loss = "absolute_error", pred_fun = pf)$M
  expect_equal(drop(s), colMeans(abs(y - pf(fit, iris))))
  
  s <- average_loss(fit, X = iris, y = y, loss = "poisson", pred_fun = pf)$M
  expect_equal(drop(s), colMeans(poisson()$dev.resid(Y, pf(fit, iris), 1)))
  
  s <- average_loss(fit, X = iris, y = y, loss = "gamma", pred_fun = pf)$M
  expect_equal(drop(s), colMeans(Gamma()$dev.resid(Y, pf(fit, iris), 1)))
})

#================================================
# Classification
#================================================

y <- cbind(iris[, 1L] > 5.8, iris[, 2L] > 3) * 1
fit <- lm(y ~ Petal.Length + Species, data = iris)
pf <- function(m, X) {
  out <- predict(m, X)
  out[out < 0] <- 0
  out[out > 1] <- 1
  out
}

test_that("average_loss() works ungrouped (multivariate binary)", {
  s <- average_loss(fit, X = iris, y = y, loss = "logloss", pred_fun = pf)$M
  expect_equal(drop(s), colMeans(binomial()$dev.resid(y, pf(fit, iris), 1)) / 2)
})

test_that("average_loss() works with groups (multivariate binary)", {
  s <- average_loss(fit, X = iris, y = y, loss = "logloss", 
                    BY = iris$Species, pred_fun = pf)$M
  xpect <- by(binomial()$dev.resid(y, pf(fit, iris), 1) / 2, 
    FUN = colMeans, INDICES = iris$Species)
  xpect <- do.call(rbind, xpect)
  colnames(s) <- colnames(xpect)
  expect_equal(s, xpect)
})

test_that("average_loss() works with weights (multivariate binary)", {
  s1 <- average_loss(fit, X = iris, y = y, 
                     pred_fun = pf, loss = "logloss")
  s2 <- average_loss(fit, X = iris, y = y, w = rep(2, times = 150), 
                     pred_fun = pf, loss = "logloss")
  s3 <- average_loss(fit, X = iris, y = y, w = iris$Petal.Width, 
                     pred_fun = pf, loss = "logloss")
  s4 <- average_loss(fit, X = iris, y = y, w = "Petal.Width", 
                     pred_fun = pf, loss = "logloss")
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
  expect_equal(s3, s4)
})

test_that("average_loss() works with weights and grouped (multi)", {
  g <- iris$Species
  s1 <- average_loss(fit, X = iris, y = y, BY = g, 
                     pred_fun = pf, loss = "logloss")
  s2 <- average_loss(fit, X = iris, y = y, w = rep(2, times = 150), BY = "Species",
                     pred_fun = pf, loss = "logloss")
  s3 <- average_loss(fit, X = iris, y = y, w = iris$Petal.Width, BY = g,
                     pred_fun = pf, loss = "logloss")
  s4 <- average_loss(fit, X = iris, y = y, w = "Petal.Width", BY = g,
                     pred_fun = pf, loss = "logloss")
  
  expect_equal(s1, s2)
  expect_false(identical(s2, s3))
  expect_equal(s3, s4)
})

test_that("mlogloss works with either matrix y or vector y", {
  pf <- function(m, X) cbind(1 - (0.1 + 0.7 * (X == 1)), 0.1 + 0.7 * (X == 1))
  X <- cbind(z = c(1, 0, 0, 1, 1))
  y <- c("B", "A", "B", "B", "B")
  Y <- model.matrix(~ y + 0)
  s1 <- average_loss(NULL, X = X, y = Y, loss = "mlogloss", pred_fun = pf)
  s2 <- average_loss(NULL, X = X, y = y, loss = "mlogloss", pred_fun = pf)
  expect_equal(s1, s2)
})

test_that("loss_mlogloss() is in line with loss_logloss() in binary case", {
  y <- iris$Species == "setosa"
  Y <- cbind(no = 1 - y, yes = y)
  fit <- glm(y ~ Sepal.Length, data = iris, family = binomial())
  pf <- function(m, X, multi = FALSE) {
    out <- predict(m, X, type = "response")
    if (multi) cbind(no = 1 - out, yes = out) else out
  }
  expect_equal(
    average_loss(fit, X = iris, y = y, pred_fun = pf, loss = "logloss", BY = y),
    average_loss(
      fit, X = iris, y = y, pred_fun = pf, loss = "mlogloss", BY = y, multi = TRUE
    )
  )
})

test_that("classification_error works on factors", {
  pf <- function(m, X) rep("setosa", times = nrow(X))
  expect_warning(average_loss(1, X = iris, y = iris$Species, pred_fun = pf))
  expect_equal(
    c(average_loss(
      1, X = iris, y = iris$Species, pred_fun = pf, loss = "classification_error"
    )$M),
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
    )$M),
    c(0, 1, 1)
  )
})

test_that("average_loss() works with missing values", {
  X <- data.frame(x1 = 1:6, x2 = c(NA, 1, 2, 1, 1, 3), x3 = factor(c("A", NA, NA, "B", "A", "A")))
  y <- 1:6
  pf <- function(fit, x) x$x1
  fit <- "a model"
  
  expect_equal(drop(average_loss(fit, X = X, y = y, pred_fun = pf)$M), 0)
  expect_warning(
    expect_warning(r <- average_loss(fit, X = X, y = y, pred_fun = pf, BY = "x3"))
  )
  expect_equal(unname(drop(r$M)), c(0, 0, 0))
  expect_s3_class(plot(r), "ggplot")
})
