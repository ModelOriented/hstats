# Univariate model
fit <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
v <- setdiff(names(iris), "Sepal.Length")
y <- iris$Sepal.Length
yy <- "Sepal.Length"
set.seed(1L)
s1 <- perm_importance(fit, X = iris[-1L], y = y, verbose = FALSE)

test_that("print() does not give error (univariate)", {
  capture_output(expect_no_error(print(s1)))
})

test_that("verbose does not produce an error", {
  expect_no_error(capture_output(
      perm_importance(fit, X = iris[-1L], y = y, verbose = TRUE)
    )
  )
})

test_that("normalize works (univariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris[-1L], y = y, normalize = TRUE, verbose = FALSE)
  perf <- average_loss(fit, X = iris, y = y)$M
  expect_equal(s1$M, s2$M * drop(perf))
  expect_equal(s1$SE, s2$SE * drop(perf))
})

test_that("v can be selected (univariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris, y = y, v = v, verbose = FALSE)
  expect_equal(s1, s2)
})

test_that("y can also be passed as name (univariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris, y = yy, verbose = FALSE)
  expect_equal(s1, s2)
})

test_that("results are positive for modeled features and zero otherwise (univariate)", {
  expect_true(all(s1$M[c("Sepal.Width", "Species"), ] > 1e-8))
  expect_true(all(s1$M[c("Petal.Length", "Petal.Width"), ] < 1e-8))
})

test_that("perm_importance() raises some errors (univariate)", {
  expect_error(perm_importance(fit, X = iris[-1L], y = 1:10, verbose = FALSE))
  expect_error(perm_importance(fit, X = iris[-1], y = "Hello", verbose = FALSE))
})

test_that("constant weights is same as unweighted (univariate)", {
  set.seed(1L)
  s2 <- perm_importance(
    fit, X = iris[-1L], y = y, w = rep(2, nrow(iris)), verbose = FALSE
  )
  expect_equal(s1, s2)
})

test_that("non-constant weights is different from unweighted (univariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris, y = yy, w = "Petal.Width", verbose = FALSE)
  
  set.seed(1L)
  s3 <- perm_importance(
    fit, 
    X = iris[-1L], 
    v = setdiff(colnames(iris[-1L]), "Petal.Width"), 
    y = y, 
    w = iris$Petal.Width, 
    verbose = FALSE
  )
  
  set.seed(1L)
  s4 <- perm_importance(
    fit, X = iris, v = colnames(iris[-1L]), y = y, w = "Petal.Width", verbose = FALSE
  )
  
  expect_false(identical(s1, s2))
  expect_identical(s2, s3)
  expect_false(identical(nrow(s2$M), nrow(s4$M)))
})

test_that("results reacts to `m_rep` (univariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris[-1L], y = y, m_rep = 100L, verbose = FALSE)
  expect_false(identical(s1$M, s2$M))
  vv <- c("Species", "Sepal.Width")
  expect_true(all(s1$SE[vv, ] > s2$SE[vv, ]))
})

test_that("perm_importance() reacts to `loss` (univariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris[-1L], y = y, loss = "gamma", verbose = FALSE)
  expect_false(identical(s1, s2))
})

test_that("perm_importance() accepts functions as losses (univariate)", {
  set.seed(1L)
  s2 <- perm_importance(
    fit, X = iris[-1L], y = y, m_rep = 2L, loss = loss_gamma, verbose = FALSE
  )
  set.seed(1L)
  s3 <- perm_importance(
    fit, X = iris[-1L], y = y, m_rep = 2L, loss = "gamma", verbose = FALSE
  )
  expect_equal(s2, s3)
})

test_that("plot() gives ggplot object (univariate)", {
  expect_s3_class(plot(s1), "ggplot")
})

test_that("Subsetting has an impact (univariate)", {
  set.seed(1L)
  s2 <- perm_importance(
    fit, X = iris[-1L], y = y, m_rep = 1L, n_max = 50, w = "Petal.Width", verbose = FALSE
  )
  set.seed(1L)
  s3 <- perm_importance(
    fit, X = iris[-1L], y = y, m_rep = 2L, n_max = 100, w = "Petal.Width", verbose = FALSE
  )
  expect_false(identical(s2, s3))
})

test_that("groups of variables work as well", {
  fit <- lm(Sepal.Length ~ ., data = iris)
  v2 <- setNames(as.list(v), v)
  v3 <- c(v2, list(petal = c("Petal.Width", "Petal.Length")))
  set.seed(1L)
  s1b <- perm_importance(fit, v = v, X = iris, y = y, verbose = FALSE)
  set.seed(1L)
  s2 <- perm_importance(fit, v = v2, X = iris, y = y, verbose = FALSE)
  expect_equal(s1b, s2)
  
  set.seed(1L)
  s3 <- perm_importance(fit, v = v3, X = iris, y = y, verbose = FALSE)
  expect_equal(s1b$M[v, , drop = FALSE], s3$M[v, , drop = FALSE])
  
  expect_equal(sort(rownames(s3$M)), sort(names(v3)))
})

test_that("matrix case works as well", {
  X <- cbind(i = 1, data.matrix(iris[2:4]))
  fit <- lm.fit(x = X, y = y)
  pred_fun <- function(m, X) X %*% m$coefficients
  expect_no_error(
    perm_importance(fit, X = X, y = y, pred_fun = pred_fun, verbose = FALSE)
  )
})

test_that("non-numeric predictions can work as well (classification error)", {
  expect_equal(
    c(perm_importance(
      1, 
      v = "Sepal.Length", 
      X = iris, 
      y = iris$Species, 
      pred_fun = function(m, X) rep("setosa", times = nrow(X)), 
      loss = "classification_error", 
      verbose = FALSE
    )$M),
    0
  )
})

#================================================
# Multivariate model
#================================================

y <- as.matrix(iris[1:2])
yy <- colnames(y)
fit <- lm(y ~ Petal.Length + Species, data = iris)
v <- c("Petal.Length", "Petal.Width", "Species")
set.seed(1L)
s1 <- perm_importance(fit, X = iris[3:5], y = y, verbose = FALSE)
perf <- average_loss(fit, X = iris, y = y)$M

test_that("print() does not give error (multivariate)", {
  capture_output(expect_no_error(print(s1)))
})

test_that("response can be passed as vector (multivariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris, y = yy, verbose = FALSE)
  expect_equal(s1, s2)
  
  set.seed(1L)
  s3 <- perm_importance(fit, X = iris, y = yy, v = colnames(iris), verbose = FALSE)
  expect_true(nrow(s2$M) < nrow(s3$M))
})

test_that("agg_cols works (multivariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris[3:5], y = y, agg_cols = TRUE, verbose = FALSE)
  expect_equal(rowSums(s1$M), drop(s2$M))
})

test_that("normalize works (multivariate, non-aggregated)", {
  i1 <- s1$M / matrix(perf, nrow = 3L, ncol = 2L, byrow = TRUE)
  
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris[3:5], y = y, normalize = TRUE, verbose = FALSE)
  i2 <- s2$M
  vv <- rownames(i1)
  expect_equal(i1, i2[vv, , drop = FALSE])
})

test_that("normalize works (multivariate, aggregated)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris[3:5], y = y, agg_cols = TRUE, verbose = FALSE)
  perf2 <- rowSums(average_loss(fit, X = iris, y = y)$M)
  i2 <- s2$M / perf2
  
  set.seed(1L)
  s3 <- perm_importance(
    fit, X = iris[3:5], y = y, normalize = TRUE, agg_cols = TRUE, verbose = FALSE
  )
  i3 <- s3$M
  expect_equal(i2, i3)
})

test_that("v can be selected (multivariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris, y = y, v = v, verbose = FALSE)
  expect_equal(s1, s2)
})

test_that("results are positive for modeled features and zero otherwise (multivariate)", {
  expect_true(all(s1$M[c("Petal.Length", "Species"), ] > 1e-8))
  expect_true(all(s1$M["Petal.Width", ] < 1e-8))
})

test_that("perm_importance() raises some errors (multivariate)", {
  expect_error(perm_importance(fit, X = iris[3:5], y = 1:10, verbose = FALSE))
  expect_error(perm_importance(fit, X = iris[3:5], y = "hi", verbose = FALSE))
})

test_that("constant weights is same as unweighted (multivariate)", {
  set.seed(1L)
  s2 <- perm_importance(
    fit, X = iris[3:5], y = y, w = rep(2, nrow(iris)), verbose = FALSE
  )
  expect_equal(s1, s2)
})

test_that("non-constant weights is different from unweighted (multivariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris[3:5], y = y, w = "Petal.Width", verbose = FALSE)
  
  set.seed(1L)
  s3 <- perm_importance(
    fit, 
    X = iris[3:5], 
    v = setdiff(colnames(iris[3:5]), "Petal.Width"), 
    y = y, 
    w = iris$Petal.Width, 
    verbose = FALSE
  )
  set.seed(1L)
  s4 <- perm_importance(
    fit, X = iris, v = colnames(iris[3:5]), y = y, w = "Petal.Width", verbose = FALSE
  )
  
  expect_false(identical(s1, s2))
  expect_identical(s2, s3)
  expect_false(identical(nrow(s2$M), nrow(s4$M)))
})

test_that("perm_importance() reacts to `m_rep` (multivariate)", {
  set.seed(1L)
  s2 <- perm_importance(fit, X = iris[3:5], y = y, m_rep = 1L, verbose = FALSE)
  expect_false(identical(s1$M, s2$M))
  expect_true(!anyNA(s1$SE))
  expect_true(all(is.na(s2$SE)))
})

test_that("perm_importance() reacts to `loss` (multivariate)", {
  set.seed(1L)
  s2 <- perm_importance(
    fit, X = iris[3:5], y = y, m_rep = 2L, loss = "gamma", verbose = FALSE
  )
  expect_false(identical(s1, s2))
})

test_that("perm_importance() accepts functions as losses (multivariate)", {
  set.seed(1L)
  s2 <- perm_importance(
    fit, X = iris[3:5], y = y, m_rep = 2L, loss = loss_gamma, verbose = FALSE
  )
  
  set.seed(1L)
  s3 <- perm_importance(
    fit, X = iris[3:5], y = y, m_rep = 2L, loss = "gamma", verbose = FALSE
  )
  expect_equal(s2, s3)
})

test_that("plot() gives ggplot object (multivariate)", {
  expect_s3_class(plot(s1, rotate_x = TRUE), "ggplot")
  expect_s3_class(plot(s1, rotate_x = TRUE, swap_dim = TRUE), "ggplot")
  expect_s3_class(plot(s1, err_type = "No"), "ggplot")
  expect_s3_class(plot(s1, err_type = "No", swap_dim = TRUE), "ggplot")
  
  s2 <- perm_importance(fit, X = iris[3:5], y = y, m_rep = 1L, verbose = FALSE)
  expect_s3_class(plot(s2), "ggplot")
  expect_s3_class(plot(s2, swap_dim = TRUE, fill = "red"), "ggplot")
})

test_that("Subsetting has an impact (multivariate)", {
  set.seed(1L)
  s2 <- perm_importance(
    fit, X = iris[3:5], y = y, m_rep = 1L, n_max = 50, w = "Petal.Width", verbose = FALSE
  )
  set.seed(1L)
  s3 <- perm_importance(
    fit, v = v, X = iris, y = y, m_rep = 2L, n_max = 100, w = "Petal.Width", verbose = FALSE
  )
  expect_false(identical(s2, s3))
})

test_that("groups of variables work as well", {
  v2 <- setNames(as.list(v), v)
  v3 <- c(v2, list(petal = c("Petal.Width", "Petal.Length")))
  set.seed(1L)
  s2 <- perm_importance(fit, v = v2, X = iris, y = y, verbose = FALSE)
  expect_equal(s1, s2)
  
  set.seed(1L)
  s3 <- perm_importance(fit, v = v3, X = iris, y = y, verbose = FALSE)
  expect_equal(s1$M[v, , drop = FALSE], s3$M[v, , drop = FALSE])
  expect_equal(sort(rownames(s3$M)), sort(names(v3)))
})

test_that("mlogloss works with either matrix y or vector y", {
  pred_fun <- function(m, X) cbind(1 - (0.1 + 0.7 * (X == 1)), 0.1 + 0.7 * (X == 1))
  X <- cbind(z = c(1, 0, 0, 1, 1))
  y <- c("B", "A", "B", "B", "B")
  Y <- model.matrix(~ y + 0)
  set.seed(1L)
  s1 <- perm_importance(
    NULL, v = "z", X = X, y = Y, loss = "mlogloss", pred_fun = pred_fun, verbose = FALSE
  )
  set.seed(1L)
  s2 <- perm_importance(
    NULL, v = "z", X = X, y = y, loss = "mlogloss", pred_fun = pred_fun, verbose = FALSE
  )
  expect_equal(s1, s2)
})

test_that("Single output multiple models works without recycling y", {
  y <- iris$Sepal.Length
  Y <- cbind(f1 = y, f2 = y)
  fit1 <- lm(y ~ Petal.Length + Species, data = iris)
  fit2 <- lm(y ~ Petal.Width + Sepal.Width, data = iris)
  fit <- list(f1 = fit1, f2 = fit2)
  pf <- function(m, x) sapply(fit, FUN = predict, newdata = x)
  
  set.seed(1L)
  s1 <- perm_importance(
    fit, X = iris[-1L], y = Y, loss = "poisson", pred_fun = pf, verbose = FALSE
  )
  set.seed(1L)
  s2 <- perm_importance(
    fit, X = iris[-1L], y = y, loss = "poisson", pred_fun = pf, verbose = FALSE
  )
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
  set.seed(1L)
  imp1 <- perm_importance(
    fit, X = iris[-5L], y = y, pred_fun = pf, loss = "logloss", verbose = FALSE
  )
  set.seed(1L)
  imp2 <- perm_importance(
    fit, X = iris[-5L], y = Y, pred_fun = pf, 
    loss = "mlogloss", multi = TRUE, verbose = FALSE
  )
  expect_equal(imp1, imp2)
})

test_that("perm_importance() works with missing values", {
  # Univariate model
  X <- data.frame(
    x1 = 1:6, x2 = c(NA, 1, 2, 1, 1, 3), x3 = factor(c("A", NA, NA, "B", "A", "A"))
  )
  y <- 1:6
  pf <- function(fit, x) x$x1
  fit <- "a model"
  
  set.seed(1L)
  expect_no_error(
    r <- perm_importance(fit, X = X, y = y, pred_fun = pf, verbose = FALSE)
  )
  expect_true(r$M[1L] > 0 && all(r$M[2:3] == 0))
})
