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
average_loss(fit, X = X_valid, y = y_valid)  # 0.023 MSE

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
library(bench)

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
bench::mark(
  iml = FeatureImp$new(mod, n.repetitions = 10, loss = "mse", compare = "difference"),
  dalex = feature_importance(ex, B = 10, type = "difference", n_sample = Inf),
  flashlight = light_importance(fl, v = x, n_max = Inf, m_repetitions = 10),
  hstats = perm_importance(fit, X = X_valid, y = y_valid, m_rep = 10, verbose = FALSE),
  check = FALSE,
  min_iterations = 3
)

#   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
# 1 iml           1.72s    1.75s     0.574   210.6MB    1.34      3     7      5.23s <NULL>
# 2 dalex      744.82ms 760.02ms     1.31     35.2MB    0.877     3     2      2.28s <NULL>
# 3 flashlight    1.29s    1.35s     0.742      63MB    0.990     3     4      4.04s <NULL>
# 4 hstats     407.26ms 412.31ms     2.43     26.5MB    0         3     0      1.23s <NULL>

# Partial dependence (cont)
v <- "tot_lvg_area"
bench::mark(
  iml = FeatureEffect$new(mod, feature = v, grid.size = 50, method = "pdp"),
  dalex = partial_dependence(ex, variables = v, N = Inf, grid_points = 50),
  flashlight = light_profile(fl, v = v, pd_n_max = Inf, n_bins = 50),
  hstats = partial_dep(fit, v = v, X = X_valid, grid_size = 50, n_max = Inf),
  check = FALSE,
  min_iterations = 3
)
#   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
# 1 iml           1.14s    1.16s     0.861   376.7MB    3.73      3    13      3.48s <NULL>
# 2 dalex      653.24ms 654.51ms     1.35    192.8MB    2.24      3     5      2.23s <NULL>
# 3 flashlight 352.34ms 361.79ms     2.72     66.7MB    0.906     3     1       1.1s <NULL>
# 4 hstats     239.03ms 242.79ms     4.04     14.2MB    1.35      3     1   743.43ms <NULL>
  
# Partial dependence (discrete)
v <- "structure_quality"
bench::mark(
  iml = FeatureEffect$new(mod, feature = v, method = "pdp", grid.points = 1:5),
  dalex = partial_dependence(ex, variables = v, N = Inf, variable_type = "categorical", grid_points = 5),
  flashlight = light_profile(fl, v = v, pd_n_max = Inf),
  hstats = partial_dep(fit, v = v, X = X_valid, n_max = Inf),
  check = FALSE,
  min_iterations = 3
)
# expression        min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time
# 1 iml         100.6ms  103.6ms      9.46   13.34MB     0        5     0      529ms <NULL>
# 2 dalex       172.4ms  177.9ms      5.62   20.55MB     2.81     2     1      356ms <NULL>
# 3 flashlight   43.5ms   45.5ms     21.9     6.36MB     2.19    10     1      457ms <NULL>
# 4 hstats       25.3ms   25.8ms     37.9     1.54MB     2.10    18     1      475ms <NULL>

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

# flashlight: 13s total, doing only one pairwise calculation, otherwise would take 63s
system.time(  # 11.5s
  fl_overall <- light_interaction(fl500, v = x, grid_size = Inf, n_max = Inf)
)
system.time(  # 2.4s
  fl_pairwise <- light_interaction(
    fl500, v = coord, grid_size = Inf, n_max = Inf, pairwise = TRUE
  )
)

# hstats: 3.5s total
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
# 1:  latitude    0.2791144
# 2: longitude    0.2791144

fl_overall$data |> subset(value > 0, select = c(variable, value))
#   variable  value
# 1 latitude  0.279
# 2 longitude 0.279

hstats_overall
# longitude  latitude 
# 0.2791144 0.2791144 

# Pairwise results match as well
iml_pairwise$results |> filter(.interaction > 1e-6)
#              .feature .interaction
# 1: longitude:latitude    0.4339574

fl_pairwise$data |> subset(value > 0, select = c(variable, value))
# latitude:longitude 0.434

hstats_pairwise
# latitude:longitude 
# 0.4339574