# test_that("pd_raw() works for simple example", {
#   X <- cbind(a = 1:2)
#   pd_raw(1, "a", X, grid = 1:2, pred_fun = function(m, x) as.vector(x))
# })

test_that("pd give same answer on example as iml 0.11.1", {
  fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
  
  # library(iml)
  # mod <- Predictor$new(fit, data = iris)
  # FeatureEffect$new(mod, feature = "Species", method = "pdp")$results
  # FeatureEffect$new(mod, feature = "Sepal.Width", method = "pdp", grid.points = 2:4)$results
  
  iml_species <- c(6.847179, 5.737053, 5.260083)
  raw_species <- c(pd_raw(fit, v = "Species", X = iris, grid = unique(iris$Species)))
  expect_equal(iml_species, raw_species, tolerance = 0.001)
  
  iml_sw <- c(5.309279, 5.814375, 6.319470)
  raw_sw <- c(pd_raw(fit, v = "Sepal.Width", X = iris, grid = 2:4))
  expect_equal(iml_sw, raw_sw, tolerance = 0.001)
  
  prof_res <- pd_profiles(fit, v = c("Species", "Sepal.Width"), X = iris, 
                          grid = list(Sepal.Width = 2:4), verbose = FALSE)
  expect_equal(summary(prof_res, "Species")$y, raw_species)
  expect_equal(summary(prof_res, "Sepal.Width")$y, raw_sw)
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