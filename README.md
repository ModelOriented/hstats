# interactML <a href='https://github.com/mayer79/interactML'><img src='man/figures/logo.png' align="right" height="139"/></a>

<!-- badges: start -->

[![CRAN status](http://www.r-pkg.org/badges/version/interactML)](https://cran.r-project.org/package=interactML)
[![R-CMD-check](https://github.com/mayer79/interactML/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mayer79/interactML/actions)
[![Codecov test coverage](https://codecov.io/gh/mayer79/interactML/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mayer79/interactML?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

[![](https://cranlogs.r-pkg.org/badges/interactML)](https://cran.r-project.org/package=interactML) 
[![](https://cranlogs.r-pkg.org/badges/grand-total/interactML?color=orange)](https://cran.r-project.org/package=interactML)

<!-- badges: end -->

## Overview

This package is about Friedman's partial dependence (PD) [1]. The main functions are as follows:

- `pd_profiles()`: PD profiles for multiple features.
- `pd_interaction()`: PD-based interaction statistics (Friedman's H [2] and variants).
- `pd_importance()`: PD-based variable importance.

The functions are 

- fast,
- support **multivariate predictions**,
- respect case weights,
- can act on matrix or data.frame data,
- and most importantly, they show progress bars :-).

## Installation

```r
# From CRAN
install.packages("interactML")

# Or the development version:
devtools::install_github("mayer79/interactML")
```

## Usage

Let's model iris flowers... :-)

### Profiles

```r
library(interactML)

fit_lm <- lm(Sepal.Width ~ . + Species:Sepal.Length, data = iris)

# Quantile grid with trimmed outliers
interactML(fit_lm, v = "Petal.Width", X = iris)

# Own grid
interactML(fit_lm, v = "Petal.Width", X = iris, grid = seq(0.1, 2.5, by = 0.1))

# Randomly selected 2D grid_
interactML(fit_lm, v = c("Species", "Petal.Width"), X = iris, grid_type = "random")
```

### Interaction

### Importance

## Background

We will first introduce some notation, partly borrowed from Friedman and Popescu [2]:


## References

1. Friedman, Jerome H., and Bogdan E. Popescu. "Predictive Learning via Rule Ensembles."
  The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
2. Friedman, Jerome H. "Greedy Function Approximation: A Gradient Boosting Machine." 
  Annals of Statistics 29 (2000): 1189-1232.
