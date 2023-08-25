# hstats 0.2.0

## Major features

- **Permutation importance**: H-statistics are often calculated only for important features. To support this workflow, we have added the function `perm_importance()`. It supports the most important loss functions. Custom losses can be passed as functions. The resulting object can be analyzed with `summary()` and `plot()`. Multivariate losses can be studied collapsed over dimensions or individually.

## Minor features

- `plot.hstats()` has recieved a `rotate_x = FALSE` argument for rotating x labels by 45 degrees.

## Bug fixes

- Probabilistic {mlr3} classifiers did not work out-of-the box. This has been fixed.

# hstats 0.1.0

This is the initial release.
