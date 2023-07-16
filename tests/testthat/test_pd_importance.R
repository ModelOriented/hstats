test_that("pd_importance() detects absent and present features (univariate)", {
  fit <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  v <- colnames(iris[-1L])
  s <- hstats(fit, v = v, X = iris, verbose = FALSE)
  expect_s3_class(pd_importance(s), "ggplot")
  imp <- pd_importance(s, plot = FALSE, sort = FALSE)
  expect_true(all(imp[c("Species", "Petal.Length"), ] > 0))
  expect_true(all(imp[c("Sepal.Width", "Petal.Width"), ] == 0))
})

test_that("pd_importance() detects absent and present features (multivariate)", {
  fit <- lm(as.matrix(iris[1:2]) ~ Species + Petal.Length, data = iris)
  v <- colnames(iris[3:5])
  s <- hstats(fit, v = v, X = iris, verbose = FALSE)
  expect_s3_class(pd_importance(s), "ggplot")
  imp <- pd_importance(s, plot = FALSE, sort = FALSE)
  expect_true(all(imp[c("Species", "Petal.Length"), ] > 0))
  expect_true(all(imp["Petal.Width", ] == 0))
})

test_that("Stronger features have higher importance", {
  pf1 <- function(m, x) 1 * x$Petal.Length + x$Petal.Width
  pf2 <- function(m, x) 2 * x$Petal.Length + x$Petal.Width 
  
  s1 <- hstats(1, colnames(iris)[3:4], X = iris, pred_fun = pf1, verbose = FALSE)
  s2 <- hstats(1, colnames(iris)[3:4], X = iris, pred_fun = pf2, verbose = FALSE)
  
  imp1 <- pd_importance(s1, plot = FALSE, sort = FALSE, top_m = 1L)
  imp2 <- pd_importance(s2, plot = FALSE, sort = FALSE, top_m = 1L)
  
  expect_true(imp2 > imp1)
})
