# hstats 0.2.0

## New major features

- **average_loss()**: This new function calculates the average loss of a model for a given dataset, optionally grouped by a discrete vector. It supports the most important loss functions (squared error, Poisson deviance, Gamma deviance, Log loss, multivariate Log loss, absolute error, classification error), and allows for case weights. Custom losses can be passed as vector/matrix valued functions of signature `f(obs, pred)`.
Note that such a custom function needs to return per-row losses, not their average.

- **perm_importance()**: H-statistics are often calculated for important features only. To support this workflow, we have added permutation importance regarding the most important loss functions. Multivariate losses can be studied individually or collapsed over dimensions. The importance of *feature groups* can be studied as well. Note that the API of `perm_importance()` is different from the experimental `pd_importance()`, which is calculated from a "hstats" object.

## Minor changes

- `summary.hstats()` now returns an object of class "summary_hstats" with its own `print()` method. Like this, one can use `su <- summary()` without printing to the console.
- The output of `summary.hstats()` is printed sligthly more compact.
- `plot.hstats()` has recieved a `rotate_x = FALSE` argument for rotating x labels by 45 degrees.
- `plot.hstats()` and `summary.hstats()` have received explicit arguments `normalize`, `squared`, `sort`, `eps` instead of passing them via `...`.
- `plot.hstats()` now passes `...` to `geom_bar()`.
- Slight speed-up of `hstats()` in the one-dimensional case.

## Bug fixes

- Probabilistic {mlr3} classifiers did not work out-of-the box. This has been fixed.

# hstats 0.1.0

This is the initial release.
