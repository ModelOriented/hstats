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

diamonds <- transform(
  diamonds,
  log_price = log(price), 
  log_carat = log(carat)
)

fit_lm <- lm(log_price ~ log_carat + clarity + color + cut, data = diamonds)

fastpdp(fit_lm)
```

## References

[1] Friedman, Jerome H. 2001. “Greedy Function Approximation: A Gradient Boosting Machine.” Ann. Statist. 29 (5): 1189–1232. https://doi.org/10.1214/aos/1013203451.
