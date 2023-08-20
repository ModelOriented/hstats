# hstats 0.2.0

## New features

- **Permutation importance**: H-statistics are often calculated only for important features. To support this workflow, we have added the function `perm_importance()`. It supports the most important loss functions, and custom losses can be passed as functions. Multivariate losses can be studied per dimension or in collapsed way. The resulting object can be analyzed with `summary()` and `plot()`.

## Bug fixes

- Probabilistic {mlr3} classifiers did not work out-of-the box. This has been fixed.

# hstats 0.1.0

This is the initial release.
