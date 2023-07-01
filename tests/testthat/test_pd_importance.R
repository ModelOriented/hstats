test_that("pd_importance() detects absent and present features (univariate)", {
  fit <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  v <- colnames(iris[-1L])
  inter <- interact(fit, v = v, X = iris, verbose = FALSE)
  expect_s3_class(pd_importance(inter), "ggplot")
  imp <- pd_importance(inter, plot = FALSE, sort = FALSE)
  expect_true(all(imp[c("Species", "Petal.Length"), ] > 0))
  expect_true(all(imp[c("Sepal.Width", "Petal.Width"), ] == 0))
})

test_that("pd_importance() detects absent and present features (multivariate)", {
  fit <- lm(as.matrix(iris[1:2]) ~ Species + Petal.Length, data = iris)
  v <- colnames(iris[3:5])
  inter <- interact(fit, v = v, X = iris, verbose = FALSE)
  expect_s3_class(pd_importance(inter), "ggplot")
  imp <- pd_importance(inter, plot = FALSE, sort = FALSE)
  expect_true(all(imp[c("Species", "Petal.Length"), ] > 0))
  expect_true(all(imp["Petal.Width", ] == 0))
})

test_that("Stronger features have higher importance", {
  pf1 <- function(m, x) 1 * x$Petal.Length + x$Petal.Width
  pf2 <- function(m, x) 2 * x$Petal.Length + x$Petal.Width 
  
  int1 <- interact(1, colnames(iris)[3:4], X = iris, pred_fun = pf1, verbose = FALSE)
  int2 <- interact(1, colnames(iris)[3:4], X = iris, pred_fun = pf2, verbose = FALSE)
  
  imp1 <- pd_importance(int1, plot = FALSE, sort = FALSE, top_m = 1L)
  imp2 <- pd_importance(int2, plot = FALSE, sort = FALSE, top_m = 1L)
  
  expect_true(imp2 > imp1)
})
