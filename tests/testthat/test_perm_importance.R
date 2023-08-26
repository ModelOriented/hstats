# Univariate model
set.seed(1L)
fit <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
v <- setdiff(names(iris), "Sepal.Length")
y <- iris$Sepal.Length
s <- perm_importance(fit, v = v, X = iris, y = y, verbose = FALSE)

test_that("print() does not give error (univariate)", {
  capture_output(expect_no_error(print(s)))
})

test_that("summary() does not give error (univariate)", {
  expect_no_error(summary(s))
})

test_that("summary() method returns statistics (univariate)", {
  su <- summary(s)
  expect_equal(drop(su$imp), s$imp)
})

test_that("summary() options have an effect (univariate)", {
  su <- summary(s, top_m = 2L)
  expect_equal(drop(su$imp), s$imp[1:2])
  
  su <- summary(s, sort = FALSE)
  expect_equal(drop(su$imp), s$imp[v])
  
  expect_equal(summary(s, agg_cols = "no"), summary(s, agg_cols = "no"))  
  expect_equal(summary(s, err_type = "sd")$err/sqrt(4), summary(s)$err)
  
  su <- summary(s, normalize = TRUE)
  expect_equal(su$imp, summary(s)$imp / s$perf)
  expect_equal(su$err, summary(s)$err / s$perf)
})

test_that("perm_importance() is positive for modeled features and zero otherwise (univariate)", {
  su <- summary(s, sort = FALSE)
  expect_equal(c(su$imp > 1e-8), c(v %in% c("Sepal.Width", "Species")))
})

test_that("perm_importance() raises some errors (univariate)", {
  expect_error(perm_importance(fit, v = v, X = iris, y = 1:10, verbose = FALSE))
})

test_that("perm_importance() with constant weights is same as unweighted (univariate)", {
  set.seed(1L)
  s1 <- perm_importance(fit, v = v, X = iris, y = y, verbose = FALSE)
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, w = rep(2, nrow(iris))
  )
  expect_equal(s1, s2)
})

test_that("perm_importance() with non-constant weights is different from unweighted (univariate)", {
  set.seed(1L)
  s1 <- perm_importance(fit, v = v, X = iris, y = y, verbose = FALSE)
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, w = 1:nrow(iris)
  )
  expect_false(identical(s1, s2))
})

test_that("perm_importance() reacts to `perms` (univariate)", {
  s <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 1L
  )
  expect_equal(dim(s$imp_raw), c(length(v), 1L, 1L))
  
  s <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L
  )
  expect_equal(dim(s$imp_raw), c(length(v), 1L, 2L))
})

test_that("perm_importance() reacts to `loss` (univariate)", {
  set.seed(1L)
  s1 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L
  )
  
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L, loss = "gamma"
  )
  expect_false(identical(s1, s2))
})

test_that("perm_importance() accepts functions as losses (univariate)", {
  set.seed(1L)
  s1 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L, loss = loss_gamma
  )
  
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L, loss = "gamma"
  )
  expect_equal(s1, s2)
})

test_that("plot() gives ggplot object (univariate)", {
  s <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 1L
  )
  expect_s3_class(plot(s), "ggplot")
  
  s <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L
  )
  expect_s3_class(plot(s), "ggplot")
})

test_that("Subsetting has an impact (univariate)", {
  set.seed(1L)
  s1 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 1L, n_max = 50
  )
  
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L, n_max = 100
  )
  
  expect_false(identical(s1, s2))
})

test_that("matrix case works as well", {
  X <- cbind(i = 1, data.matrix(iris[2:4]))
  fit <- lm.fit(x = X, y = y)
  pred_fun <- function(m, X) X %*% m$coefficients
  expect_no_error(
    perm_importance(
      fit, v = colnames(iris[2:4]), X = X, y = y, pred_fun = pred_fun, verbose = FALSE
    )
  )
})

#================================================
# Multivariate model
#================================================

y <- as.matrix(iris[1:2])
fit <- lm(y ~ Petal.Length + Species, data = iris)
v <- c("Petal.Length", "Petal.Width", "Species")
s <- perm_importance(fit, v = v, X = iris, y = iris[1:2], verbose = FALSE)

test_that("print() does not give error (multivariate)", {
  capture_output(expect_no_error(print(s)))
})

test_that("summary() does not give error (multivariate)", {
  expect_no_error(summary(s))
})

test_that("summary() method returns statistics (multivariate)", {
  su <- summary(s)
  expect_equal(drop(su$imp), s$imp)
})

test_that("summary() options have an effect (multivariate, aggregated)", {
  su <- summary(s, top_m = 2L)
  expect_equal(drop(su$imp), s$imp[1:2])
  
  su <- summary(s, sort = FALSE)
  expect_equal(drop(su$imp), s$imp[v])
  
  expect_equal(summary(s, agg_cols = "sum")$imp / 2, summary(s, agg_cols = "mean")$imp)  
  expect_equal(summary(s, err_type = "sd")$err/sqrt(4), summary(s)$err)
  
  su <- summary(s, normalize = TRUE)
  expect_equal(su$imp, summary(s)$imp / sum(s$perf))
  expect_equal(su$err, summary(s)$err / sum(s$perf))
})

test_that("summary() options have an effect (multivariate, non-aggregated)", {
  su <- summary(s, top_m = 2L, agg_cols = "no")
  expect_equal(rowSums(su$imp), s$imp[1:2])
  
  su <- summary(s, sort = FALSE, agg_cols = "no")
  expect_equal(rowSums(su$imp), s$imp[v])
  
  expect_equal(
    summary(s, err_type = "sd", agg_cols = "no")$err / sqrt(4), 
    summary(s, agg_cols = "no")$err
  )
  
  su <- summary(s, normalize = TRUE, agg_cols = "no")
  expect_equal(
    su$imp, 
    sweep(summary(s, agg_cols = "no")$imp, MARGIN = 2L, STATS = s$perf, FUN = "/")
  )
  expect_equal(
    su$err, 
    sweep(summary(s, agg_cols = "no")$err, MARGIN = 2L, STATS = s$perf, FUN = "/")
  )
})

test_that("perm_importance() is positive for modeled features and zero otherwise (multivariate)", {
  su <- summary(s, sort = FALSE)
  expect_equal(c(su$imp > 1e-8), c(v %in% c("Petal.Length", "Species")))
})

test_that("perm_importance() raises some errors (multivariate)", {
  expect_error(perm_importance(fit, v = v, X = iris, y = 1:10, verbose = FALSE))
})

test_that("perm_importance() with constant weights is same as unweighted (multivariate)", {
  set.seed(1L)
  s1 <- perm_importance(fit, v = v, X = iris, y = y, verbose = FALSE)
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, w = rep(2, nrow(iris))
  )
  expect_equal(s1, s2)
})

test_that("perm_importance() with non-constant weights is different from unweighted (multivariate)", {
  set.seed(1L)
  s1 <- perm_importance(fit, v = v, X = iris, y = y, verbose = FALSE)
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, w = 1:nrow(iris)
  )
  expect_false(identical(s1, s2))
})

test_that("perm_importance() reacts to `perms` (multivariate)", {
  s <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 1L
  )
  expect_equal(dim(s$imp_raw), c(length(v), 2L, 1L))
  
  s <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 3L
  )
  expect_equal(dim(s$imp_raw), c(length(v), 2L, 3L))
})

test_that("perm_importance() reacts to `loss` (multivariate)", {
  set.seed(1L)
  s1 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L
  )
  
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L, loss = "gamma"
  )
  expect_false(identical(s1, s2))
})

test_that("perm_importance() accepts functions as losses (multivariate)", {
  set.seed(1L)
  s1 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L, loss = loss_gamma
  )
  
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L, loss = "gamma"
  )
  expect_equal(s1, s2)
})

test_that("plot() gives ggplot object (multivariate)", {
  s <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 1L
  )
  expect_s3_class(plot(s, rotate_x = TRUE), "ggplot")
  expect_s3_class(plot(s, agg_cols = "no", err_type = "no"), "ggplot")
  
  s <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L
  )
  expect_s3_class(plot(s), "ggplot")
  expect_s3_class(plot(s, agg_cols = "no"), "ggplot")
})

test_that("Subsetting has an impact (multivariate)", {
  set.seed(1L)
  s1 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 1L, n_max = 50
  )
  
  set.seed(1L)
  s2 <- perm_importance(
    fit, v = v, X = iris, y = y, verbose = FALSE, perms = 2L, n_max = 100
  )
  
  expect_false(identical(s1, s2))
})

test_that("mlogloss works with either matrix y or vector y", {
  pred_fun <- function(m, X) cbind(1 - (0.1 + 0.7 * (X == 1)), 0.1 + 0.7 * (X == 1))
  X <- cbind(z = c(1, 0, 0, 1, 1))
  y <- c("B", "A", "B", "B", "B")
  Y <- model.matrix(~ y + 0)
  set.seed(1L)
  s1 <- perm_importance(
    NULL, v = "z", X = X, y = Y, verbose = FALSE, loss = "mlogloss", pred_fun = pred_fun
  )
  
  set.seed(1L)
  s2 <- perm_importance(
    NULL, v = "z", X = X, y = y, verbose = FALSE, loss = "mlogloss", pred_fun = pred_fun
  )
  
  expect_equal(s1, s2)
})


