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
# expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
# iml           1.26s    1.29s     0.778   235.3MB     3.63     3    14
# dalex      594.39ms 620.76ms     1.63     30.5MB     1.63     3     3
# flashlight  800.1ms  808.4ms     1.20     62.6MB     3.20     3     8
# hstats     151.06ms 160.19ms     5.94       26MB     1.98     3     1

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
# expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
# iml         921.7ms  923.5ms      1.06   369.7MB     3.18     3     9
# dalex       503.2ms  516.7ms      1.90   195.1MB     3.80     3     6
# flashlight  190.3ms  228.9ms      4.62    67.9MB     1.54     3     1
# hstats       74.6ms   76.8ms     12.2     14.1MB     1.75     7     1

# Partial dependence (discrete)
v <- "structure_quality"
bench::mark(
  iml = FeatureEffect$new(mod, feature = v, method = "pdp", grid.points = 1:5),
  dalex = partial_dependence(ex, variables = v, N = Inf, variable_type = "categorical", grid_points = 5),
  flashlight = light_profile(fl, v = v, pd_n_max = Inf),
  hstats = partial_dep(fit, v = v, X = X_valid, n_max = Inf),
  check = FALSE,
  min_iterations = 10
)
# expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc
# iml          77.1ms   80.6ms     12.4    12.66MB     8.25     6     4
# dalex       122.4ms  137.7ms      7.49   20.48MB     7.49     5     5
# flashlight   25.8ms   27.5ms     34.0      6.3MB     2.13    16     1
# hstats       14.3ms   15.1ms     66.1     1.49MB     2.13    31     1

# H-Stats -> we use a subset of 500 rows
X_v500 <- X_valid[1:500, ]
mod500 <- Predictor$new(fit, data = as.data.frame(X_v500), predict.function = predf)
fl500 <- flashlight(fl, data = as.data.frame(valid[1:500, ]))

# iml 173 s total, using slow exact calculations (to be able to compare results)
system.time(  # 85s
  iml_overall <- Interaction$new(mod500, grid.size = 500)
)
system.time(  # 88s for all combinations of latitude
  iml_pairwise <- Interaction$new(mod500, grid.size = 500, feature = "latitude")
)

# flashlight: 8s total, doing only one pairwise calculation, otherwise would take 63s
system.time(  # 7s
  fl_overall <- light_interaction(fl500, v = x, grid_size = Inf, n_max = Inf)
)
system.time(  # 1s
  fl_pairwise <- light_interaction(
    fl500, v = coord, grid_size = Inf, n_max = Inf, pairwise = TRUE
  )
)

# hstats: 1.5s total
system.time({
  H <- hstats(fit, v = x, X = X_v500, n_max = Inf)
  hstats_overall <- h2_overall(H, squared = FALSE, zero = FALSE)
  hstats_pairwise <- h2_pairwise(H, squared = FALSE, zero = FALSE)
}
)

# Using 50 quantiles to approximate dense numerics: 0.5s
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
  subset(value > 0, select = c(variable, value))
#   variable_    value_
# 3  latitude 0.246
# 4 longitude 0.246

hstats_overall
# longitude  latitude 
# 0.2458269 0.2458269 

# Pairwise results match as well
iml_pairwise$results |>
  filter(.interaction > 1e-6)
#              .feature .interaction
# 1: longitude:latitude    0.3942526

fl_pairwise$data |>
  subset(value > 0, select = c(variable, value))
# latitude:longitude 0.394

hstats_pairwise
# latitude:longitude 
# 0.3942526