test_that("Additive models show 0 interactions (univariate)", {
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

test_that("Additive models show 0 interactions (multivariate)", {
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

test_that("Non-additive models show interactions > 0 (one interaction)", {
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
  expect_s3_class(plot(inter), "ggplot")
})

fit <- lm(
  Sepal.Width ~ . + Petal.Length:Petal.Width + Petal.Length:Species, data = iris
)
v <- setdiff(colnames(iris), "Sepal.Width")
inter <- interact(fit, v = v, X = iris, verbose = FALSE)

test_that("Non-additive models show interactions > 0 (two interactions)", {
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

test_that("Case weights have an impact", {
  inter1 <- inter
  inter1$w <- NULL
  inter2 <- interact(fit, v = v, X = iris, verbose = FALSE, w = rep(2, times = 150L))
  inter2[["w"]] <- NULL
  expect_equal(inter1, inter2)
  
  capture_output(inter2 <- interact(fit, v = v, X = iris, w = 1:150))
  inter2[["w"]] <- NULL
  expect_false(identical(inter1, inter2))
})

test_that("print() method does not give error", {
  capture_output(expect_no_error(print(inter)))
})

test_that("summary() method returns statistics", {
  capture_output(expect_no_error(s <- summary(inter)))
  expect_equal(s$H2, H2(inter))
  expect_equal(s$H2_j, H2_j(inter, plot = FALSE, top_m = Inf))
  expect_equal(s$H2_jk, H2_jk(inter, plot = FALSE, top_m = Inf))
})

test_that("Stronger interactions get higher statistics", {
  pf1 <- function(m, x) x$Petal.Width + x$Petal.Width * x$Petal.Length
  pf2 <- function(m, x) x$Petal.Width + x$Petal.Width * x$Petal.Length * 2
  
  int1 <- interact(1, colnames(iris)[-1L], X = iris, pred_fun = pf1, verbose = FALSE)
  int2 <- interact(1, colnames(iris)[-1L], X = iris, pred_fun = pf2, verbose = FALSE)
  
  expect_true(H2(int2) > H2(int1))
  expect_true(
    all(H2_j(int2, plot = FALSE, top_m = 2L) > H2_j(int1, plot = FALSE, top_m = 2L))
  )
  expect_true(H2_jk(int2, plot = FALSE) > H2_jk(int1, plot = FALSE))
})

test_that("subsampling has an effect", {
  set.seed(1L)
  out1 <- interact(fit, v = v, X = iris, n_max = 10L, w = 1:150, verbose = FALSE)
  
  set.seed(2L)
  out2 <- interact(fit, v = v, X = iris, n_max = 10L, w = 1:150, verbose = FALSE)
  
  expect_false(identical(out1, out2))
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
