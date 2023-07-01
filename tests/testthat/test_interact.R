test_that("Additive models get right answer in univariate situation", {
  fit <- lm(Sepal.Width ~ ., data = iris)
  v <- setdiff(colnames(iris), "Sepal.Width")
  inter <- interact(fit, v = v, X = iris, verbose = FALSE)
  expect_null(H2_jk(inter))
  expect_equal(
    H2_j(inter, plot = FALSE), 
    matrix(c(0, 0, 0, 0), ncol = 1L, dimnames = list(v, NULL))
  )
  expect_equal(H2(inter), 0)
  expect_s3_class(H2_j(inter), "ggplot")
})

test_that("Additive models get right answer in multivariate situation", {
  fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
  v <- c("Petal.Length", "Petal.Width", "Species")
  inter <- interact(fit, v = v, X = iris, verbose = FALSE)
  expect_null(H2_jk(inter))
  expect_equal(
    H2_j(inter, plot = FALSE), 
    matrix(
      0, ncol = 2L, nrow = 3L, dimnames = list(v, c("Sepal.Length", "Sepal.Width"))
    )
  )
  expect_equal(H2(inter), c(Sepal.Length = 0, Sepal.Width = 0))
  expect_s3_class(H2_j(inter), "ggplot")
})

test_that("H-stats detect single pairwise interaction", {
  fit <- lm(Sepal.Width ~ . + Petal.Length:Petal.Width, data = iris)
  v <- setdiff(colnames(iris), "Sepal.Width")
  inter <- interact(fit, v = v, X = iris, verbose = FALSE)
  
  expect_true(H2(inter) > 0)
  
  out <- H2_j(inter, plot = FALSE)
  expect_equal(rownames(out[out > 0, , drop = FALSE]), c("Petal.Length", "Petal.Width"))

  out <- H2_jk(inter, plot = FALSE)
  expect_equal(rownames(out), "Petal.Length:Petal.Width")
  
  expect_s3_class(H2_j(inter), "ggplot")
  expect_s3_class(H2_jk(inter), "ggplot")
})

test_that("H-stats detect two pairwise interactions", {
  fit <- lm(
    Sepal.Width ~ . + Petal.Length:Petal.Width + Petal.Length:Species, data = iris
  )
  v <- setdiff(colnames(iris), "Sepal.Width")
  inter <- interact(fit, v = v, X = iris, verbose = FALSE)
  
  expect_true(H2(inter) > 0)
  
  out <- H2_j(inter, plot = FALSE, sort = FALSE, normalize = FALSE, squared = FALSE)
  expect_equal(
    rownames(out[out > 0, , drop = FALSE]), 
    c("Petal.Length", "Petal.Width", "Species")
  )
  
  out <- H2_jk(inter, plot = FALSE, sort = FALSE, normalize = FALSE, squared = FALSE)
  expect_equal(
    rownames(out[out > 0, , drop = FALSE]), 
    c("Petal.Length:Petal.Width", "Petal.Length:Species")
  )
  
  expect_s3_class(H2_j(inter), "ggplot")
  expect_s3_class(H2_jk(inter), "ggplot")
})

#
# library(gbm)
#
# fit <- gbm(Sepal.Length ~ ., data = iris, interaction.depth = 3, bag.fraction = 1)
# v <- names(iris)[-1]
# combs <- combn(v, 2, simplify = FALSE)
# p <- length(combs)
#
# res <- setNames(numeric(p), sapply(combs, paste, collapse = ":"))
# for (i in 1:p) {
#   res[i] <- interact.gbm(fit, iris, i.var = combs[[i]], n.trees = fit$n.trees)
# }
# cbind(res[res > 0.0001])
# # Sepal.Width:Petal.Length 0.10982072
# # Sepal.Width:Petal.Width  0.17932506
# # Sepal.Width:Species      0.21480383
# # Petal.Length:Petal.Width 0.03702921
# # Petal.Length:Species     0.06382609
#
# # Crunching
# system.time( # 0.3 s
#   inter <- i_compute(fit, v = v, X = iris, n.trees = fit$n.trees)
# )
# H2_jk(inter, squared = FALSE, sort = FALSE)
# # Sepal.Width:Petal.Length 0.10532810
# # Sepal.Width:Petal.Width  0.16697609
# # Sepal.Width:Species      0.17335494
# # Petal.Length:Petal.Width 0.03245863
# # Petal.Length:Species     0.06678683
#
