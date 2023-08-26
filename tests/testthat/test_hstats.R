test_that("Additive models show 0 interactions (univariate)", {
  fit <- lm(Sepal.Width ~ ., data = iris)
  v <- setdiff(colnames(iris), "Sepal.Width")
  s <- hstats(fit, v = v, X = iris, verbose = FALSE)
  expect_null(h2_pairwise(s))
  expect_null(h2_threeway(s))
  expect_equal(
    h2_overall(s, plot = FALSE), 
    matrix(c(0, 0, 0, 0), ncol = 1L, dimnames = list(v, NULL))
  )
  expect_equal(h2(s), 0)
  expect_s3_class(h2_overall(s), "ggplot")
  expect_s3_class(plot(s, rotate_x = TRUE), "ggplot")
})

test_that("Additive models show 0 interactions (multivariate)", {
  fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
  v <- c("Petal.Length", "Petal.Width", "Species")
  s <- hstats(fit, v = v, X = iris, verbose = FALSE)
  expect_null(h2_pairwise(s))
  expect_null(h2_threeway(s))
  expect_equal(
    h2_overall(s, plot = FALSE), 
    matrix(
      0, ncol = 2L, nrow = 3L, dimnames = list(v, c("Sepal.Length", "Sepal.Width"))
    )
  )
  expect_equal(h2(s), c(Sepal.Length = 0, Sepal.Width = 0))
  expect_s3_class(h2_overall(s), "ggplot")
  expect_s3_class(plot(s), "ggplot")
})

test_that("Non-additive models show interactions > 0 (one interaction)", {
  fit <- lm(Sepal.Width ~ . + Petal.Length:Petal.Width, data = iris)
  v <- setdiff(colnames(iris), "Sepal.Width")
  s <- hstats(fit, v = v, X = iris, verbose = FALSE)
  
  expect_true(h2(s) > 0)
  
  out <- h2_overall(s, plot = FALSE)
  expect_equal(rownames(out[out > 0, , drop = FALSE]), c("Petal.Length", "Petal.Width"))

  out <- h2_pairwise(s, plot = FALSE)
  expect_equal(rownames(out), "Petal.Length:Petal.Width")
  
  expect_s3_class(h2_overall(s), "ggplot")
  expect_s3_class(h2_pairwise(s), "ggplot")
  expect_s3_class(plot(s), "ggplot")
  
  expect_null(h2_threeway(s))
})

fit <- lm(
  Sepal.Width ~ . + Petal.Length:Petal.Width + Petal.Length:Species, data = iris
)
v <- setdiff(colnames(iris), "Sepal.Width")
s <- hstats(fit, v = v, X = iris, verbose = FALSE)

test_that("Non-additive models show interactions > 0 (two interactions)", {
  expect_true(h2(s) > 0)
  
  out <- h2_overall(s, plot = FALSE, sort = FALSE, normalize = FALSE, squared = FALSE)
  expect_equal(
    rownames(out[out > 0, , drop = FALSE]), 
    c("Petal.Length", "Petal.Width", "Species")
  )
  
  out <- h2_pairwise(s, plot = FALSE, sort = FALSE, normalize = FALSE, squared = FALSE)
  expect_equal(
    rownames(out[out > 0, , drop = FALSE]), 
    c("Petal.Length:Petal.Width", "Petal.Length:Species")
  )
  
  expect_s3_class(h2_overall(s), "ggplot")
  expect_s3_class(h2_pairwise(s), "ggplot")
  expect_s3_class(h2_threeway(s), "ggplot")
  
  expect_equal(c(h2_threeway(s, plot = FALSE)), 0)
})

test_that("Case weights have an impact", {
  s1 <- s
  s1$w <- NULL
  s2 <- hstats(fit, v = v, X = iris, verbose = FALSE, w = rep(2, times = 150L))
  s2[["w"]] <- NULL
  expect_equal(s1, s2)
  
  capture_output(s2 <- hstats(fit, v = v, X = iris, w = 1:150))
  s2[["w"]] <- NULL
  expect_false(identical(s1, s2))
})

test_that("print() method does not give error", {
  capture_output(expect_no_error(print(s)))
})

test_that("summary() method returns statistics", {
  capture_output(expect_no_error(sm <- summary(s)))
  expect_equal(sm$h2, h2(s))
  expect_equal(sm$h2_overall, h2_overall(s, plot = FALSE, top_m = Inf))
  expect_equal(sm$h2_pairwise, h2_pairwise(s, plot = FALSE, top_m = Inf))
  expect_equal(sm$h2_threeway, h2_threeway(s, plot = FALSE, top_m = Inf))
})

test_that("Stronger interactions get higher statistics", {
  pf1 <- function(m, x) x$Petal.Width + x$Petal.Width * x$Petal.Length
  pf2 <- function(m, x) x$Petal.Width + x$Petal.Width * x$Petal.Length * 2
  
  int1 <- hstats(1, colnames(iris)[-1L], X = iris, pred_fun = pf1, verbose = FALSE)
  int2 <- hstats(1, colnames(iris)[-1L], X = iris, pred_fun = pf2, verbose = FALSE)
  
  expect_true(h2(int2) > h2(int1))
  expect_true(
    all(h2_overall(int2, plot = FALSE, top_m = 2L) > h2_overall(int1, plot = FALSE, top_m = 2L))
  )
  expect_true(h2_pairwise(int2, plot = FALSE) > h2_pairwise(int1, plot = FALSE))
})

test_that("subsampling has an effect", {
  set.seed(1L)
  out1 <- hstats(fit, v = v, X = iris, n_max = 10L, w = 1:150, verbose = FALSE)
  
  set.seed(2L)
  out2 <- hstats(fit, v = v, X = iris, n_max = 10L, w = 1:150, verbose = FALSE)
  
  expect_false(identical(out1, out2))
})

test_that("multivariate results are consistent", {
  fit <- lm(cbind(up = uptake, up2 = 2 * uptake) ~ Type * Treatment * conc, data = CO2)
  s <- hstats(fit, v = names(CO2[2:4]), X = CO2, verbose = FALSE)
  
  # Normalized
  out <- h2_pairwise(s, plot = FALSE)
  expect_equal(out[, "up"], out[, "up2"])
  
  # Unnormalized
  out <- h2_pairwise(s, plot = FALSE, normalize = FALSE, squared = FALSE)
  expect_equal(2 * out[, "up"], out[, "up2"])
})

test_that("Three-way interaction is positive in model with such terms", {
  fit <- lm(uptake ~ Type * Treatment * conc, data = CO2)
  s <- hstats(fit, v = names(CO2[2:4]), X = CO2, verbose = FALSE)
  expect_true(h2_threeway(s, plot = FALSE) > 0)
})

test_that("Three-way interaction behaves correctly across dimensions", {
  fit <- lm(cbind(up = uptake, up2 = 2 * uptake) ~ Type * Treatment * conc, data = CO2)
  s <- hstats(fit, v = names(CO2[2:4]), X = CO2, verbose = FALSE)
  out <- h2_threeway(s, plot = FALSE)
  expect_equal(out[, "up"], out[, "up2"])
  out <- h2_threeway(s, plot = FALSE, squared = FALSE, normalize = FALSE)
  expect_equal(2 * out[, "up"], out[, "up2"])
})

test_that("get_v() works", {
  H <- cbind(c(a = 1, b = 3, c = 2, d = 5))
  expect_equal(get_v(H, 2L), c("b", "d"))
  expect_equal(get_v(H, 1L), "d")
  
  H <- cbind(
    x = c(a = 1, b = 3, c = 2, d = 5),
    y = 4:1
  )
  expect_equal(get_v(H, 2L), c("a", "b", "d"))
  expect_equal(get_v(H, 1L), c("a", "d"))
})

test_that("matrix case works as well", {
  X <- cbind(i = 1, data.matrix(iris[2:4]))
  fit <- lm.fit(x = X, y = iris$Sepal.Length)
  pred_fun <- function(m, X) X %*% m$coefficients
  s <- hstats(
    fit, v = colnames(iris[2:4]), X = X, pred_fun = pred_fun, verbose = FALSE
  )
  expect_equal(c(h2_overall(s, plot = FALSE)), c(0, 0, 0))
})

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
#   s <- hstats(fit, v = v, X = iris, n.trees = fit$n.trees)
# )
# h2_pairwise(s, squared = FALSE, sort = FALSE)
# # Sepal.Width:Petal.Length 0.10532810
# # Sepal.Width:Petal.Width  0.16697609
# # Sepal.Width:Species      0.17335494
# # Petal.Length:Petal.Width 0.03245863
# # Petal.Length:Species     0.06678683
#
