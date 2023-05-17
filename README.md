# fastpdp <a href='https://github.com/mayer79/fastpdp'><img src='man/figures/logo.png' align="right" height="139"/></a>

<!-- badges: start -->

[![CRAN status](http://www.r-pkg.org/badges/version/fastpdp)](https://cran.r-project.org/package=fastpdp)
[![R-CMD-check](https://github.com/mayer79/fastpdp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mayer79/fastpdp/actions)
[![Codecov test coverage](https://codecov.io/gh/mayer79/fastpdp/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mayer79/fastpdp?branch=main)

[![](https://cranlogs.r-pkg.org/badges/fastpdp)](https://cran.r-project.org/package=fastpdp) 
[![](https://cranlogs.r-pkg.org/badges/grand-total/fastpdp?color=orange)](https://cran.r-project.org/package=fastpdp)

<!-- badges: end -->

## Overview

## Installation

```r
# From CRAN
install.packages("fastpdp")

# Or the development version:
devtools::install_github("mayer79/fastpdp")
```

## Usage

Let's model iris flowers, as usual :-).

### Linear regression

```r
library(fastpdp)

fit_lm <- lm(Sepal.Width ~ ., data = iris)

# Quantile grid with trimmed outliers
fastpdp(fit_lm, v = "Petal.Width", X = iris)

# Own grid
fastpdp(fit_lm, v = "Petal.Width", X = iris, grid = seq(0.1, 2.5, by = 0.1))

# Randomly selected 2D grid_
fastpdp(fit_lm, v = c("Species", "Petal.Width"), X = iris, grid_type = "random")
```

### Random forest

```r
library(ranger)

fit_rf <- ranger(Species ~ ., data = iris, probability = TRUE)

fastpdp(fit_rf, v = "Sepal.Width", X = iris)
```

### Deep neural net

Or a deep neural net (results not fully reproducible):

```r
library(keras)

y <- iris[, 1]
X <- data.matrix(iris[2:5])

nn <- keras_model_sequential()
nn |>
  layer_dense(units = 30, activation = "relu", input_shape = 4) |>
  layer_dense(units = 15, activation = "relu") |>
  layer_dense(units = 1)

nn |>
  compile(optimizer = optimizer_adam(0.05), loss = "mse")

cb <- list(
  callback_early_stopping(patience = 20),
  callback_reduce_lr_on_plateau(patience = 5)
)
       
nn |>
  fit(
    x = X,
    y = y,
    epochs = 100,
    batch_size = 10, 
    validation_split = 0.2,
    callbacks = cb
  )

fastpdp(nn, v = "Species", X = X, batch_size = 1000)

```

## Meta-learning packages

Here, we provide some working examples for "tidymodels", "caret", and "mlr3".

### tidymodels

```r
library(tidymodels)
library(fastpdp)

iris_recipe <- iris %>%
  recipe(Sepal.Length ~ .)

reg <- linear_reg() %>%
  set_engine("lm")
  
iris_wf <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(reg)

fit <- iris_wf %>%
  fit(iris)
  
fastpdp(fit, v = "Species", X = iris)
```

### caret

```r
library(caret)
library(fastpdp)

fit <- train(
  Sepal.Length ~ ., 
  data = iris, 
  method = "lm", 
  tuneGrid = data.frame(intercept = TRUE),
  trControl = trainControl(method = "none")
)

fastpdp(fit, v = "Species", X = iris)
```

### mlr3

```r
library(mlr3)
library(mlr3learners)
library(fastpdp)

mlr_tasks$get("iris")
tsk("iris")
task_iris <- TaskRegr$new(id = "iris", backend = iris, target = "Sepal.Length")
fit <- lrn("regr.lm")
fit$train(task_iris)

fastpdp(fit, v = "Species", X = iris)
```

## References

[1] Friedman, Jerome H. 2001. “Greedy Function Approximation: A Gradient Boosting Machine.” Ann. Statist. 29 (5): 1189–1232. https://doi.org/10.1214/aos/1013203451.
