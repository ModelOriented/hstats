# hstats 0.2.0

## New major features

- **average_loss()**: This new function calculates the average loss over a pair (X, y), optionally grouped by a discrete vector. It supports the most important loss functions (squared error, Poisson deviance, Gamma deviance, Log loss, multivariate Log loss, absolute error), and allows for case weights. Custom losses can be passed as vector/matrix valued functions of signature `f(obs, pred)`.

- **perm_importance()**: H-statistics are often calculated for important features only. To support this workflow, we have added permutation importance. It supports the same loss functions as `average_loss()`, including custom losses. Multivariate losses can be studied individually or collapsed over dimensions. The API is different from the experimental `pd_importance()`.

## Minor improvements

- `plot.hstats()` has recieved a `rotate_x = FALSE` argument for rotating x labels by 45 degrees.
- Slight speed-up of `hstats()` in the one-dimensional case.

## Bug fixes

- Probabilistic {mlr3} classifiers did not work out-of-the box. This has been fixed.

# hstats 0.1.0

This is the initial release.
