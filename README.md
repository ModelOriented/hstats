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

Let's model diamonds prices!

### Linear regression

```r
library(ggplot2)
library(fastpdp)

fit_lm <- lm(price ~ carat + clarity + color + cut, data = diamonds)

# Quantile grid with trimmed outliers
fastpdp(fit_lm, v = "carat", X = diamonds)

# Own grid
fastpdp(fit_lm, v = "carat", X = diamonds, grid = seq(0.2, 2.6, by = 0.1))
```

### Random forest

```r
library(ranger)

fit_rf <- ranger(
  price ~ carat + clarity + color + cut, data = diamonds
)

fastpdp(fit_rf, v = "clarity", X = diamonds)
```

### Deep neural net

Or a deep neural net (results not fully reproducible):

```r
library(keras)

x <- c("carat", "clarity", "color", "cut")

nn <- keras_model_sequential()
nn |>
  layer_dense(units = 30, activation = "relu", input_shape = 4) |>
  layer_dense(units = 15, activation = "relu") |>
  layer_dense(units = 1)

nn |>
  compile(optimizer = optimizer_adam(0.5), loss = "mse")

cb <- list(
  callback_early_stopping(patience = 20),
  callback_reduce_lr_on_plateau(patience = 5)
)
       
nn |>
  fit(
    x = data.matrix(diamonds[x]),
    y = diamonds$price,
    epochs = 100,
    batch_size = 400, 
    validation_split = 0.2,
    callbacks = cb
  )

fastpdp(nn, v = "clarity", X = data.matrix(diamonds[x]), batch_size = 1000)

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
  
ks <- kernelshap(fit, iris[, -1], bg_X = iris)
ks
```

### caret

```r
library(caret)
library(kernelshap)
library(shapviz)

fit <- train(
  Sepal.Length ~ ., 
  data = iris, 
  method = "lm", 
  tuneGrid = data.frame(intercept = TRUE),
  trControl = trainControl(method = "none")
)

s <- kernelshap(fit, iris[, -1], predict, bg_X = iris)
sv <- shapviz(s)
sv_waterfall(sv, 1)
```

### mlr3

```r
library(mlr3)
library(mlr3learners)
library(kernelshap)
library(shapviz)

mlr_tasks$get("iris")
tsk("iris")
task_iris <- TaskRegr$new(id = "iris", backend = iris, target = "Sepal.Length")
fit_lm <- lrn("regr.lm")
fit_lm$train(task_iris)
s <- kernelshap(fit_lm, iris[-1], bg_X = iris)
sv <- shapviz(s)
sv_dependence(sv, "Species")
```

## References

[1] Friedman, Jerome H. 2001. “Greedy Function Approximation: A Gradient Boosting Machine.” Ann. Statist. 29 (5): 1189–1232. https://doi.org/10.1214/aos/1013203451.
