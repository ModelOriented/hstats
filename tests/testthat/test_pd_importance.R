test_that("pd_importance() detects absent and present features (univariate)", {
  fit <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
  s <- hstats(fit, X = iris[-1L], verbose = FALSE)
  imp <- pd_importance(s, sort = FALSE)
  expect_true(all(imp$M[c("Species", "Petal.Length"), ] > 0))
  expect_true(all(imp$M[c("Sepal.Width", "Petal.Width"), ] == 0))
  expect_s3_class(plot(imp), "ggplot")
})

test_that("pd_importance() detects absent and present features (multivariate)", {
  fit <- lm(as.matrix(iris[1:2]) ~ Species + Petal.Length, data = iris)
  s <- hstats(fit, X = iris[3:5], verbose = FALSE)
  imp <- pd_importance(s, sort = FALSE)
  expect_s3_class(plot(imp), "ggplot")
  expect_s3_class(plot(imp, swap_dim = TRUE), "ggplot")
  expect_true(all(imp$M[c("Species", "Petal.Length"), ] > 0))
  expect_true(all(imp$M["Petal.Width", ] == 0))
})

test_that("Stronger features have higher importance", {
  pf1 <- function(m, x) 1 * x$Petal.Length + x$Petal.Width
  pf2 <- function(m, x) 2 * x$Petal.Length + x$Petal.Width 
  
  s1 <- hstats(1, X = iris[3:4], pred_fun = pf1, verbose = FALSE)
  s2 <- hstats(1, X = iris[3:4], pred_fun = pf2, verbose = FALSE)
  
  imp1 <- pd_importance(s1, sort = FALSE)
  imp2 <- pd_importance(s2, sort = FALSE)
  
  expect_true(imp2$M[1L, ] > imp1$M[1L, ])
})
