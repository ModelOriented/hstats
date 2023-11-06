library(hstats)
library(shapviz)
library(xgboost)
library(ggplot2)

# Data preparation
colnames(miami) <- tolower(colnames(miami))
miami <- transform(miami, log_price = log(sale_prc))
x <- c("tot_lvg_area", "lnd_sqfoot", "latitude", "longitude",
       "structure_quality", "age", "month_sold")
coord <- c("longitude", "latitude")

# Modeling
set.seed(1)
ix <- sample(nrow(miami), 0.8 * nrow(miami))
train <- data.frame(miami[ix, ])
valid <- data.frame(miami[-ix, ])
y_train <- train$log_price
y_valid <- valid$log_price
X_train <- data.matrix(train[x])
X_valid <- data.matrix(valid[x])

dtrain <- xgb.DMatrix(X_train, label = y_train)
dvalid <- xgb.DMatrix(X_valid, label = y_valid)

ic <- c(
  list(which(x %in% coord) - 1),
  as.list(which(!x %in% coord) - 1)
)

# Fit via early stopping
fit <- xgb.train(
  params = list(
    learning_rate = 0.15, 
    objective = "reg:squarederror", 
    max_depth = 5,
    interaction_constraints = ic
  ),
  data = dtrain,
  watchlist = list(valid = dvalid),
  early_stopping_rounds = 20,
  nrounds = 1000,
  callbacks = list(cb.print.evaluation(period = 100))
)

# Interpret via {hstats}
average_loss(fit, X = X_valid, y = y_valid)  # 0.0247 MSE -> 0.157 RMSE

perm_importance(fit, X = X_valid, y = y_valid) |> 
  plot()

# Or combining some features
v_groups <- list(
  coord = c("longitude", "latitude"),
  size = c("lnd_sqfoot", "tot_lvg_area"),
  condition = c("age", "structure_quality")
)
perm_importance(fit, v = v_groups, X = X_valid, y = y_valid) |> 
  plot()

H <- hstats(fit, v = x, X = X_valid)
H
plot(H)
plot(H, zero = FALSE)
h2_pairwise(H, zero = FALSE, squared = FALSE, normalize = FALSE)
partial_dep(fit, v = "tot_lvg_area", X = X_valid) |> 
  plot()
partial_dep(fit, v = "tot_lvg_area", X = X_valid, BY = "structure_quality") |> 
  plot(show_points = FALSE)
plot(ii <- ice(fit, v = "tot_lvg_area", X = X_valid))
plot(ii, center = TRUE)

# Spatial plots
g <- unique(X_valid[, coord])
pp <- partial_dep(fit, v = coord, X = X_valid, grid = g)
plot(pp, d2_geom = "point", alpha = 0.5, size = 1) + 
  coord_equal()

# Takes some seconds because it generates the last plot per structure quality
partial_dep(fit, v = coord, X = X_valid, grid = g, BY = "structure_quality") |>
  plot(pp, d2_geom = "point", alpha = 0.5) +
  coord_equal()

#=====================================
# Naive benchmark
#=====================================

library(iml)  # Might benefit of multiprocessing, but on Windows with XGB models, this is not easy
library(DALEX)
library(ingredients)
library(flashlight)
library(microbenchmark)

set.seed(1)

# iml
predf <- function(object, newdata) predict(object, data.matrix(newdata[x]))
mod <- Predictor$new(fit, data = as.data.frame(X_valid), y = y_valid, 
                     predict.function = predf)

# DALEX
ex <- DALEX::explain(fit, data = X_valid, y = y_valid)

# flashlight (my slightly old fashioned package)
fl <- flashlight(
  model = fit, data = valid, y = "log_price", predict_function = predf, label = "lm"
)
  
# Permutation importance: 10 repeats over full validation data (~2700 rows)
microbenchmark(
  iml = FeatureImp$new(mod, n.repetitions = 10, loss = "mse", compare = "difference"),
  dalex = feature_importance(ex, B = 10, type = "difference", n_sample = Inf),
  flashlight = light_importance(fl, v = x, n_max = Inf, m_repetitions = 10),
  hstats = perm_importance(fit, X = X_valid, y = y_valid, m_rep = 10, verbose = FALSE),
  times = 4
)
 
# Unit: milliseconds
# expr             min        lq      mean    median        uq       max neval  cld
# iml        1532.8545 1535.5962 1578.6838 1538.3425 1621.7713 1705.1956     4 a  
# dalex       599.6996  623.6306  673.3109  660.4653  722.9913  772.6134     4  b 
# flashlight  602.7707  619.6451  670.2922  639.5598  720.9393  799.2784     4  b 
# hstats      379.4195  381.1585  414.5308  410.3719  447.9031  457.9601     4   c

# Partial dependence (cont)
v <- "tot_lvg_area"
microbenchmark(
  iml = FeatureEffect$new(mod, feature = v, grid.size = 50, method = "pdp"),
  dalex = partial_dependence(ex, variables = v, N = Inf, grid_points = 50),
  flashlight = light_profile(fl, v = v, pd_n_max = Inf, n_bins = 50),
  hstats = partial_dep(fit, v = v, X = X_valid, grid_size = 50, n_max = Inf),
  times = 4
)
# Unit: milliseconds
# expr            min       lq     mean    median        uq       max neval  cld
# iml        941.7763 968.5576 993.0481 1002.5849 1017.5386 1025.2462     4 a   
# dalex      694.8007 740.1619 767.1501  788.6172  794.1384  796.5654     4  b  
# flashlight 327.6056 328.7617 330.4069  330.5388  332.0522  332.9445     4   c 
# hstats     216.4040 217.0602 217.5606  217.8603  218.0611  218.1179     4    d

# Partial dependence (discrete)
v <- "structure_quality"
microbenchmark(
  iml = FeatureEffect$new(mod, feature = v, method = "pdp", grid.points = 1:5),
  dalex = partial_dependence(ex, variables = v, N = Inf, variable_type = "categorical", grid_points = 5),
  flashlight = light_profile(fl, v = v, pd_n_max = Inf),
  hstats = partial_dep(fit, v = v, X = X_valid, n_max = Inf),
  times = 4
)

# Unit: milliseconds
# expr            min        lq      mean    median        uq      max neval  cld
# iml         90.3690  91.08965  94.18403  92.57250  97.27840 101.2221     4 a   
# dalex      174.2517 174.97330 179.43483 175.87115 183.89635 191.7453     4  b  
# flashlight  43.9318  45.05070  48.09375  46.64275  51.13680  55.1577     4   c 
# hstats      24.5972  24.64975  25.01325  24.94085  25.37675  25.5741     4    d

# H-Stats -> we use a subset of 500 rows
X_v500 <- X_valid[1:500, ]
mod500 <- Predictor$new(fit, data = as.data.frame(X_v500), predict.function = predf)
fl500 <- flashlight(fl, data = as.data.frame(valid[1:500, ]))

# iml  # 225s total, using slow exact calculations
system.time(  # 90s
  iml_overall <- Interaction$new(mod500, grid.size = 500)
)
system.time(  # 135s for all combinations of latitude
  iml_pairwise <- Interaction$new(mod500, grid.size = 500, feature = "latitude")
)

# flashlight: 14s total, doing only one pairwise calculation, otherwise would take 63s
system.time(  # 12s
  fl_overall <- light_interaction(fl500, v = x, grid_size = Inf, n_max = Inf)
)
system.time(  # 2s
  fl_pairwise <- light_interaction(
    fl500, v = coord, grid_size = Inf, n_max = Inf, pairwise = TRUE
  )
)

# hstats: 3s total
system.time({
  H <- hstats(fit, v = x, X = X_v500, n_max = Inf)
  hstats_overall <- h2_overall(H, squared = FALSE, zero = FALSE)
  hstats_pairwise <- h2_pairwise(H, squared = FALSE, zero = FALSE)
}
)

# Using 50 quantiles to approximate dense numerics: 0.9s
system.time(
  H_approx <- hstats(fit, v = x, X = X_v500, n_max = Inf, approx = TRUE)
)

# Overall statistics correspond exactly
iml_overall$results |> filter(.interaction > 1e-6)
#     .feature .interaction
# 1:  latitude    0.2458269
# 2: longitude    0.2458269

fl_overall$data |> subset(value > 0, select = c(variable, value))
#   variable  value
# 1 latitude  0.246
# 2 longitude 0.246

hstats_overall
# longitude  latitude 
# 0.2458269 0.2458269 

# Pairwise results match as well
iml_pairwise$results |> filter(.interaction > 1e-6)
#              .feature .interaction
# 1: longitude:latitude    0.3942526

fl_pairwise$data |> subset(value > 0, select = c(variable, value))
# latitude:longitude 0.394

hstats_pairwise
# latitude:longitude 
# 0.3942526 