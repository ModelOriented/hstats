# fastpdp <a href='https://github.com/mayer79/fastpdp'><img src='man/figures/logo.png' align="right" height="139"/></a>

<!-- badges: start -->

[![CRAN status](http://www.r-pkg.org/badges/version/fastpdp)](https://cran.r-project.org/package=fastpdp)
[![R-CMD-check](https://github.com/mayer79/fastpdp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mayer79/fastpdp/actions)
[![Codecov test coverage](https://codecov.io/gh/mayer79/fastpdp/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mayer79/fastpdp?branch=main)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

[![](https://cranlogs.r-pkg.org/badges/fastpdp)](https://cran.r-project.org/package=fastpdp) 
[![](https://cranlogs.r-pkg.org/badges/grand-total/fastpdp?color=orange)](https://cran.r-project.org/package=fastpdp)

<!-- badges: end -->

## Overview

This package is about Friedman's partial dependence (PD) [1]. The main functions are as follows:

- `pd_profiles()`: PD profiles for multiple features.
- `pd_interaction()`: PD-based interaction statistics (Friedman's H [2] and variants).
- `pd_importance()`: PD-based variable importance.

They are 

- fast,
- support **multivariate predictions** (classification and multi-target models),
- and most importantly, they show progress bars :-).

## Installation

```r
# From CRAN
install.packages("fastpdp")

# Or the development version:
devtools::install_github("mayer79/fastpdp")
```

## Usage

Let's model iris flowers... :-)

### Profiles

```r
library(fastpdp)

fit_lm <- lm(Sepal.Width ~ . + Species:Sepal.Length, data = iris)

# Quantile grid with trimmed outliers
fastpdp(fit_lm, v = "Petal.Width", X = iris)

# Own grid
fastpdp(fit_lm, v = "Petal.Width", X = iris, grid = seq(0.1, 2.5, by = 0.1))

# Randomly selected 2D grid_
fastpdp(fit_lm, v = c("Species", "Petal.Width"), X = iris, grid_type = "random")
```

### Interaction

### Importance

## Background

## References

1. Friedman, Jerome H. "Greedy Function Approximation: A Gradient Boosting Machine." 
  Annals of Statistics 29 (2000): 1189-1232.
2. Friedman, Jerome H., and Bogdan E. Popescu. "Predictive Learning via Rule Ensembles."
  The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
3. Greenwell, Brandon M., Bradley C. Boehmke and Andrew J. McCarthy. 
  "A Simple and Effective Model-Based Variable Importance Measure." 
  ArXiv abs/1805.04755 (2018)
  
