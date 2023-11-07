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
# expr        min        lq      mean    median        uq       max neval cld
# iml        1610.4464 1622.3517 1657.6455 1642.3422 1692.9394 1735.4514     4 a  
# dalex       580.5633  628.7967  665.1718  685.7349  701.5470  708.6542     4  b 
# flashlight  622.3130  630.7167  648.9690  648.5589  667.2214  676.4453     4  b 
# hstats      332.1432  334.4255  337.0738  337.0140  339.7221  342.1240     4   c

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
# expr             min        lq      mean    median        uq       max neval  cld
# iml        1098.6226 1111.7868 1123.7506 1129.6484 1135.7144 1137.0828     4 a   
# dalex       740.6559  762.3050  827.4134  784.8789  892.5218  999.2398     4  b  
# flashlight  363.2473  368.0095  392.9258  373.7185  417.8420  461.0187     4   c 
# hstats      213.4137  214.0246  225.5381  224.5216  237.0517  239.6956     4    d

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
# expr      min        lq      mean    median        uq      max neval  cld
# iml         96.5188  96.84865 101.15893  99.27995 105.46920 109.5570     4 a   
# dalex      166.5767 167.09505 169.68585 169.94295 172.27665 172.2808     4  b  
# flashlight  40.8074  41.76215  49.22383  44.56515  56.68550  66.9576     4   c 
# hstats      23.7283  23.86510  24.99588  24.01180  26.12665  28.2316     4    d

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

# hstats: 3.4s total
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