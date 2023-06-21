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

**What makes a ML model black-box? It's the interactions!**

This package offers a fast, model-agnostic implementation of Friedman and Popescu's statistics of interaction strength [2]. As such, it helps to unveil the darkness of the black-box.

The package

- supports multivariate predictions,
- respects case weights, and
- works with both data.frames and matrices (e.g., for XGBoost).
- Furthermore, different variants of the original statistics in [2] are available.

Note: The {gbm} package offers a model-specific implementation of some of the statistics. Since it uses the weighted tree-traversal method of [1] to estimate partial dependence functions, the results are typically slightly different.

## Installation

```r
# From CRAN
install.packages("interactML")

# Or the development version:
devtools::install_github("mayer79/interactML")
```

## Usage

To demonstrate the typical workflow, we use a beautiful house price dataset with about 14,000 transactions from Miami-Dade County available in the {shapviz} package. 

We are going to model logarithmic sales prices as a function of geographic features and other features like living area and building age. The model is fitted with XGBoost using interaction constraints to produce a model additive in all non-geographic features.

What can we say about interactions? Can we verify additivity in non-geographic features?

```r
library(interactML)
library(xgboost)
library(shapviz)

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

We will now do two things:

1. Call `interact()` for the expensive crunching.
2. Get main statistics with `summary()`.

```r
# Crunch
set.seed(1)

system.time(  # 2-3 seconds on simple laptop - a random forest will take much longer
  inter <- interact(fit, v = x, X = X_train)
)

summary(inter)

# Output
Proportion of prediction variability explained by interactions:
         y 
0.09602024 

Features with strongest overall interactions (Friedman and Popescu's H^2):
                            y
OCEAN_DIST        0.062639450
LONGITUDE         0.045194768
LATITUDE          0.029151873
CNTR_DIST         0.027695769
RAIL_DIST         0.003805603
HWY_DIST          0.003484982
TOT_LVG_AREA      0.000000000
LND_SQFOOT        0.000000000
structure_quality 0.000000000
age               0.000000000

Feature pairs with strong interactions (Friedman and Popescu's H^2):
                               y
LONGITUDE:OCEAN_DIST 0.156475264
LONGITUDE:CNTR_DIST  0.122113602
LATITUDE:LONGITUDE   0.076213847
LATITUDE:OCEAN_DIST  0.061130256
CNTR_DIST:OCEAN_DIST 0.040675335
LATITUDE:RAIL_DIST   0.030786973
LATITUDE:CNTR_DIST   0.029474763
CNTR_DIST:RAIL_DIST  0.012886536
LONGITUDE:RAIL_DIST  0.008402545
OCEAN_DIST:RAIL_DIST 0.007081773
```

**Comments:** 

- About 10% of prediction variability comes from interaction effects.
- The strongest overall interactions are associated with "OCEAN_DIST" and "LONGITUDE". For instance, we can say that about 6% of prediction variability can be attributed to all interactions of "OCEAN_DISTANCE".
- About 15.6% of the joint effect variability of above two features come from their pairwise interaction.

Remarks: 

1. Pairwise statistics are calculated only for the features with strongest overall interactions.
2. Pairwise Friedmans and Popescu's $H^2_{jk}$ measures interaction strength relative to the combined effect of the two features. As a modification, we can compute a variant with common denominator (overall prediction variability, as with $H^2_j$), see below.
3. The statistics need to repeatedly calculate predictions on $n^2$ rows. That is why {interactML} samples 300 rows by default. To get more robust results, increase this value at the price of slower run time.

Which pairwise interaction is strongest?

```r
H2_jk(inter, denominator = "F", top_m = 5)
                               y
LONGITUDE:OCEAN_DIST 0.024027352
LATITUDE:OCEAN_DIST  0.008067966
LONGITUDE:CNTR_DIST  0.008063092
CNTR_DIST:OCEAN_DIST 0.007805464
LATITUDE:LONGITUDE   0.005081128
```

**Comment:** It remains the interaction between Longitude and distance to the ocean. The value tells us that about 2.4% of overall prediction variability comes from this interaction.

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

If there are no interactions involving $x_j$, we can decompose the prediction function $F$ into the sum of the partial dependence $F_j$ on $x_j$ and the partial dependence $F_{\setminus j}$ on all other features $\boldsymbol x_{\setminus j}$, i.e.,

$$
	F(\boldsymbol x) = F_j(x_j) + F_{\setminus j}(\boldsymbol x_{\setminus j}).
$$

Correspondingly, Friedman and Popescu's $H^2_j$ statistic of overall interaction strength is given by

$$
	H_{j}^2 = \frac{\frac{1}{n} \sum_{i = 1}^n\big[F(\boldsymbol x_i) - \hat F_j(x_{ij}) - \hat F_{\setminus j}(\boldsymbol x_{i\setminus k})\big]^2}{\frac{1}{n} \sum_{i = 1}^n\big[F(\boldsymbol x_i)\big]^2}.
$$

**Remarks**

1. Partial dependence functions (and $F$) are all centered to mean 0.
2. Partial dependence functions (and $F$) are evaluated over the data distribution. This is different to partial dependence plots, where one uses a fixed grid.
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

**Modification**

To be able to compare pairwise interaction strength across variable pairs, and to overcome the problem mentioned in the last remark, we suggest as alternative a different denominator, namely the same as used for $H^2_j$:

$$
  \tilde H^2_{jk} = \frac{A_{jk}}{\frac{1}{n} \sum_{i = 1}^n \left[F(\boldsymbol x_i)\right]^2}.
$$

This statistic measures how much of the total variance of the predictions comes from the pairwise interaction of $x_j$ and $x_k$.

Another possibility would be to use the unnormalized test statistic on the scale of the predictions, i.e., $\sqrt{A_{jk}}$.

### Total interaction strength of all variables together

If the model is additive in all features (no interactions), then

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

1. Evaluate the statistics only on a subset of the data, e.g., on $n' = 300$ observations.
2. Calculate $H_j^2$ for all features. Then, select a small number $m = O(\sqrt{p})$ of features with highest $H^2_j$ and do pairwise calculations only on this subset.

This leads to a total number of $O(n'^2 p)$ predictions.

## References

1. Friedman, Jerome H. *"Greedy Function Approximation: A Gradient Boosting Machine."* 
  Annals of Statistics 29, no. 5 (2001): 1189-1232.
2. Friedman, Jerome H., and Bogdan E. Popescu. *"Predictive Learning via Rule Ensembles."*
  The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
3. Mayer, Michael, Steven C. Bourassa, Martin Hoesli, and Donato Scognamiglio. *"Machine Learning Applications to Land and Structure Valuation."* Journal of Risk and Financial Management 15, no. 5 (2022): 193.
