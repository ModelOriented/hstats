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

**What makes a ML model black-box? It is the complex interactions!**

This package offers a fast model-agnostic implementation of Friedman and Popescu's statistics of interaction strength [2]. As such, it helps to unveil the darkness of the black-box.

The package

- supports multivariate predictions,
- respects case weights, and
- works with both data.frames and matrices (e.g., for XGBoost).

Note: The numbers usually slightly differ from the implementation in the {gbm} package. The reason is that {interactML} is model-agnostic and therefore cannot rely on the fast tree-traversal method to calculate partial dependence functions.

## Installation

```r
# From CRAN
install.packages("interactML")

# Or the development version:
devtools::install_github("mayer79/interactML")
```

## Usage

To demonstrate the typical workflow, we use a house price dataset with 14,000 transactions from Miami-Date county, available in the {shapviz} package. We model logarithmic sales prices as a function of geographic features and other features like living area and building age. The model is fitted with XGBoost, using interaction constraints to produce a model additive in all non-geographic features.

Let's first fit such model: 

```r
library(xgboost)
library(shapviz)
library(interactML)

# Variable sets
x_geo <- c("LATITUDE", "LONGITUDE", "CNTR_DIST", "OCEAN_DIST", "RAIL_DIST", "HWY_DIST")
x_nongeo <- c("TOT_LVG_AREA", "LND_SQFOOT", "structure_quality", "age")
x <- c(x_geo, x_nongeo)

# Build interaction constraint vector
ic <- c(
  list(which(x %in% x_geo) - 1),
  as.list(which(x %in% x_nongeo) - 1)
)

# Train/valid split
set.seed(1)
ix <- sample(nrow(miami), 0.8 * nrow(miami))

y_train <- log(miami$SALE_PRC[ix])
y_valid <- log(miami$SALE_PRC[-ix])
X_train <- data.matrix(miami[ix, x])
X_valid <- data.matrix(miami[-ix, x])

dtrain <- xgb.DMatrix(X_train, label = y_train)
dvalid <- xgb.DMatrix(X_valid, label = y_valid)

params <- list(
  learning_rate = 0.2,
  objective = "reg:squarederror",
  max_depth = 5,
  interaction_constraints = ic
)

fit <- xgb.train(
  params = params,
  data = dtrain,
  watchlist = list(valid = dvalid),
  early_stopping_rounds = 20,
  nrounds = 1000,
  callbacks = list(cb.print.evaluation(period = 100))
)
```

Now, we will go through two main steps:

1. Calculate interaction strength per feature.
2. Calculate pairwise interactions, but only for those features with strongest interactions in Step 1.

By default, {interactML} subsamples 400 rows to do the calculations of all statistics. Predictions are done on cross-products, i.e., on datasets with 1'600'000 rows.

```r
# Crunch
set.seed(1)

system.time(  # 4 seconds on simple laptop
  inter <- interact(fit, v = x, X = X_train)
)

# The rest of the calculations are derived from "inter"

# Friedman and Popescu's H-squared statistic of overall interaction strength
H2_overall(inter)

# Output
# OCEAN_DIST        0.064581466
# LONGITUDE         0.047399654
# CNTR_DIST         0.029462043
# LATITUDE          0.029256430
# RAIL_DIST         0.003820832
# HWY_DIST          0.002992241
# TOT_LVG_AREA      0.000000000
# LND_SQFOOT        0.000000000
# structure_quality 0.000000000
# age               0.000000000

# Friedman and Popescu's H-squared statistic of pairwise interaction strength
H2_pairwise(inter)

# Output
# LONGITUDE:OCEAN_DIST 0.152528992
# LONGITUDE:CNTR_DIST  0.123840925
# LATITUDE:LONGITUDE   0.078635069
# LATITUDE:OCEAN_DIST  0.053268299
# CNTR_DIST:OCEAN_DIST 0.041599895
# LATITUDE:RAIL_DIST   0.028792788
# LATITUDE:CNTR_DIST   0.027796538
# CNTR_DIST:RAIL_DIST  0.012492165
# LONGITUDE:RAIL_DIST  0.007827756
# OCEAN_DIST:RAIL_DIST 0.006477485

# Variant that uses a common denominator for fair comparison of interactions
H2_pairwise(inter, denominator = "f")
                                y
# LONGITUDE:OCEAN_DIST 0.0255737064
# CNTR_DIST:OCEAN_DIST 0.0085670987
# LONGITUDE:CNTR_DIST  0.0085552233
# LATITUDE:OCEAN_DIST  0.0076632078
# LATITUDE:LONGITUDE   0.0052859289
# LATITUDE:CNTR_DIST   0.0037626874
# OCEAN_DIST:RAIL_DIST 0.0006404859
# CNTR_DIST:RAIL_DIST  0.0006234004
# LATITUDE:RAIL_DIST   0.0005841129
# LONGITUDE:RAIL_DIST  0.0001973087

# Overall proportion of variability explained by interactions
total_interaction(inter)  # 0.095
```

**Comments:** The model indeed is additive in non-geographic features, i.e., the interaction constraints are respected. Furthermore, we see that about 10% of prediction variation comes from interaction effects. The clearly strongest interaction happens between longitude and distance to the ocean.

## Background

### Partial dependence

Let $F: R^p \to R$ denote the prediction function that maps the $p$-dimensional feature vector $\boldsymbol x = (x_1, \dots, x_p)$ to its prediction.
Furthermore, let $F_s(\boldsymbol x_s) = E_{\boldsymbol x_{\setminus s}}(F(\boldsymbol x_s, \boldsymbol x_{\setminus s}))$ be the partial dependence function of $F$ on the feature subset $\boldsymbol x_s$, where $s \subseteq \{1, \dots, p\}$, as introduced in [1]. Here, the expectation runs over the joint marginal distribution of features $\boldsymbol x_{\setminus s}$ not in $\boldsymbol x_s$.

Given data, $F_s(\boldsymbol x_s)$ can be estimated by the empirical partial dependence function

$$
  \hat F_s(\boldsymbol x_s) = \frac{1}{n} \sum_{i = 1}^n F(\boldsymbol x_s, \boldsymbol x_{i \setminus s}),
$$

where $\boldsymbol x_{i\setminus s}$, $i = 1, \dots, n$, are the observed values of $\boldsymbol x_{\setminus s}$.

### Overall interaction strength

In [2], Friedman and Popescu introduced different statistics to measure interaction strength. Closely following their notation, we will summarize the main ideas. 

If there are no interactions involving $x_j$, we can decompose the prediction function $F$ as the sum of the partial dependence $F_j$ on $x_j$ and the partial dependence $F_{\setminus j}$ on all other features $\boldsymbol x_{\setminus j}$, i.e.,

$$
	F(\boldsymbol x) = F_j(x_j) + F_{\setminus j}(\boldsymbol x_{\setminus j}).
$$

Correspondingly, Friedman and Popescu's $H^2_j$ statistic of overall interaction strength is given by

$$
	H_{j}^2 = \frac{\frac{1}{n} \sum_{i = 1}^n\big[F(\boldsymbol x_i) - \hat F_j(x_{ij}) - \hat F_{\setminus j}(\boldsymbol x_{i\setminus k})\big]^2}{\frac{1}{n} \sum_{i = 1}^n\big[F(\boldsymbol x_i)\big]^2}.
$$

**Remarks**

1. Partial dependence functions are all centered to mean 0.
2. Partial dependence functions are evaluated over the data distribution. This is different to partial dependence plots, where one uses a fixed grid.
3. Weighted versions follow by replacing all arithmetic means by corresponding weighted means.
4. Multivariate predictions can be treated in a component-wise manner.
5. $H^2_j = 0$ means there are no interactions associated with $x_j$. The higher the value, the more prediction variability comes from interactions with $x_j$.
6. Since the denominator is the same for all features, the values of the test statistics can be compared across features.

### Pairwise interaction strength

Again following [2], if there are no interaction effects between features $x_j$ and $x_k$, their two-dimensional partial dependence function $F_{jk}$ can be written as the sum of the univariate partial dependencies, i.e.,

$$
  F_{jk}(x_j, x_k) = F_j(x_j)+ F_k(x_k).
$$

Correspondingly, Friedman and Popescu's $H_{jk}^2$ statistic of pairwise interaction strength is defined as

$$
  H_{jk}^2 = \frac{A_{jk}}{B_{jk}},
$$

where 

$$
  A_{jk} = \frac{1}{n} \sum_{i = 1}^n\big[\hat F_{jk}(x_{ij}, x_{ik}) - \hat F_j(x_{ij}) - \hat F_k(x_{ik})\big]^2
$$

and

$$
  B_{jk} = \frac{1}{n} \sum_{i = 1}^n\big[\hat F_{jk}(x_{ij}, x_{ik})\big]^2.
$$ 

**Remarks**

1. Remarks 1 to 4 of $H^2_{j}$ also apply here.
2. $H^2_{jk} = 0$ means there are no interaction effects between $x_j$ and $x_k$. The larger the value, the more of the joint effect of the two features comes from the interaction.
3.  Since the denominator differs between variable pairs, unlike $H_j$, this test statistic is difficult to compare between variable pairs. If both main effects are very weak, a negligible interaction can get a high $H^2_{jk}$.

**Alternatives**

To be able to compare pairwise interaction strength across variable pairs, and to overcome the problem mentioned in the last remark, we suggest as alternative a different denominator, namely the same as used for $H^2_j$:

$$
  \tilde H^2_{jk} = \frac{A_{jk}}{\frac{1}{n} \sum_{i = 1}^n \left[F(\boldsymbol x_i)\right]^2}.
$$

This statistic would tell us how much of the total variance of the predictions comes from the pairwise interaction of $x_j$ and $x_k$.

Another possibility would be to use the unnormalized test statistic on the scale of the predictions, i.e., $\sqrt{A_{jk}}$.

### Total interaction strength of all variables together

In the spirit of [2], we can say: if the model is additive in all features (no interactions), then

$$
	F(\boldsymbol x) = \sum_{j}^{p} F_j(x_j).
$$

To measure the relative amount of variability explained by all interactions, we can therefore study the test statistic of total interaction strength

$$
  H^2 = \frac{\frac{1}{n} \sum_{i = 1}^n \left[F(\boldsymbol x_i) - \sum_{j = 1}^p\hat F_j(x_{ij})\right]^2}{\frac{1}{n} \sum_{i = 1}^n\left[F(\boldsymbol x_i)\right]^2}.
$$

It equals the variability of the predictions unexplained by the main effects. A value of 0 would mean there are no interaction effects at all.

### Workflow

Calculation of all $H_j^2$ statistics requires $O(n^2 p)$ predictions, while calculating of all pairwise $H_{jk}$ requires $O(n^2 p^2$ predictions. Therefore, we suggest to reduce the workflow in two important ways:

1. Evaluate the statistics only on a subset of the data, e.g., on $n' = 400$ observations.
2. Calculate $H_j^2$ for all features. Then, select a small number $m = O(\sqrt{p})$ of features with highest $H^2_j$ and do pairwise calculations only on this subset.

This leads to a total number of $O(n'^2 p)$ predictions.

## References

1. Friedman, Jerome H. *"Greedy Function Approximation: A Gradient Boosting Machine."* 
  Annals of Statistics 29, no. 5 (2001): 1189-1232.
2. Friedman, Jerome H., and Bogdan E. Popescu. *"Predictive Learning via Rule Ensembles."*
  The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
3. Mayer, Michael, Steven C. Bourassa, Martin Hoesli, and Donato Scognamiglio. *"Machine Learning Applications to Land and Structure Valuation."* Journal of Risk and Financial Management 15, no. 5 (2022): 193.
