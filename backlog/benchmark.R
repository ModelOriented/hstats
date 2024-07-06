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
#   expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result
# 1 iml           1.76s    1.76s     0.565   211.6MB    3.39      3    18      5.31s <NULL>
# 2 dalex      688.54ms 697.71ms     1.44     35.2MB    1.91      3     4      2.09s <NULL>
# 3 flashlight 667.51ms 676.07ms     1.47     28.1MB    1.96      3     4      2.04s <NULL>
# 4 hstats     392.15ms 414.41ms     2.39     26.6MB    0.796     3     1      1.26s <NULL>  

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
#     expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result
# <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>
#   1 iml            1.2s     1.4s     0.726   376.9MB     4.12     3    17      4.13s <NULL>
#   2 dalex       759.3ms  760.6ms     1.28    192.8MB     2.55     3     6      2.35s <NULL>
#   3 flashlight  369.1ms  403.1ms     2.55     66.8MB     2.55     3     3      1.18s <NULL>
#   4 hstats      242.1ms  243.8ms     4.03     14.2MB     0        3     0   744.25ms <NULL>#   
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
#     expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result
# <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>
#   1 iml         107.9ms    108ms      9.26   13.64MB     9.26     2     2      216ms <NULL>
#   2 dalex         172ms  172.2ms      5.81   21.14MB     2.90     2     1      344ms <NULL>
#   3 flashlight   40.3ms   41.6ms     23.8     8.61MB     2.16    11     1      462ms <NULL>
#   4 hstats       24.5ms   25.9ms     35.5     1.64MB     0       18     0      507ms <NULL>
  
# H-Stats -> we use a subset of 500 rows
X_v500 <- X_valid[1:500, ]
mod500 <- Predictor$new(fit, data = as.data.frame(X_v500), predict.function = predf)
fl500 <- flashlight(fl, data = as.data.frame(valid[1:500, ]))

# iml  # 243s total, using slow exact calculations
system.time(  # 110s
  iml_overall <- Interaction$new(mod500, grid.size = 500)
)
system.time(  # 133s for all combinations of latitude
  iml_pairwise <- Interaction$new(mod500, grid.size = 500, feature = "latitude")
)

# flashlight: 14s total, doing only one pairwise calculation, otherwise would take 63s
system.time(  # 11.7s
  fl_overall <- light_interaction(fl500, v = x, grid_size = Inf, n_max = Inf)
)
system.time(  # 2.3s
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

# Using 50 quantiles to approximate dense numerics: 0.8s
system.time(
  H_approx <- hstats(fit, v = x, X = X_v500, n_max = Inf, approx = TRUE)
)

# Overall statistics correspond exactly
iml_overall$results |>
  filter(.interaction > 1e-6)
#     .feature .interaction
# 1:  latitude    0.2458269
# 2: longitude    0.2458269

fl_overall$data |>
  subset(value_ > 0, select = c(variable_, value_))
#   variable_    value_
# 3  latitude 0.2458269
# 4 longitude 0.2458269

hstats_overall
# longitude  latitude 
# 0.2458269 0.2458269 

# Pairwise results match as well
iml_pairwise$results |>
  filter(.interaction > 1e-6)
#              .feature .interaction
# 1: longitude:latitude    0.3942526

fl_pairwise$data |>
  subset(value_ > 0, select = c(variable_, value_))
# latitude:longitude 0.3942526

hstats_pairwise
# latitude:longitude 
# 0.3942526