# hstats 1.0.1

## Enhancements

- `hstats()`, `partial_dep()`, `ice()` now also work for factor predictions. They are represented as one-hot-encoded vectors.
- Use of a much faster way to one-hot-encode factor predictions, written by Mathias Ambühl.
- The plot method of a two-dimensional PDP has recieved the option `d2_geom = "line"`. Instead of a heatmap of the two features, one of the features is moved to color grouping. This might give a better impression where the interaction happens. Combined with `swap_dim = TRUE`, you can swap the role of the two `v` variables without recalculating anything. The idea was proposed by [Roel Verbelen](https://github.com/RoelVerbelen) in [issue #91](https://github.com/mayer79/hstats/issues/91), see also [issue #94](https://github.com/mayer79/hstats/issues/94).

## Bug fixes

- Using `BY` and `w` via column names would fail for tibbles. This problem was described in [#92](https://github.com/mayer79/hstats/issues/92) by [Roel Verbelen](https://github.com/RoelVerbelen). Thx!

## Other changes

- Add unit tests to compare against {iml}.
- Made all examples "tibble" and "data.table" friendly.

# hstats 1.0.0

## Major changes

- Quantile approximation: `hstats()` now has the option `approx = FALSE`. Set to `TRUE` to replace values of dense numeric columns by `grid_size = 50` quantile midpoints. This will bring a massive speed-up for one-way calculations. Use this option when one-way calculations are slow, or when you want to increase `n_max`.
- `hstats()`: `n_max` has been increased from 300 to 500 rows. This will make estimates of H-statistics more stable at the price of longer run time. Reduce to 300 for the old behaviour.
- `hstats()`: Three-way interactions are not anymore calculated by default. Set `threeway_m` to 5 for the old behaviour.
- Revised plots: The colors and color palettes have changed and can now also be controlled via global options. For instance, to change the fill color of all bars, set `options(hstats.fill = new value)`. Value labels are more clear, and there are more options. Varying color/fill scales now use viridis (inferno). This can be modified on the fly or via `options(hstats.viridis_args = list(...))`.
- "hstats_matrix" object: All statistics functions, e.g., `h2_pairwise()` or `perm_importance()`, now return a "hstats_matrix". The values are stored in `$M` and can be plotted via `plot()`. Other methods include: `dimnames()`, `rownames()`, `colnames()`, `dim()`, `nrow()`, `ncol()`, `head()`, `tail()`, and subsetting like a normal matrix. This allows, e.g, to select and plot only one column of the results.
- `perm_importance()`: The `perms` argument has been changed to `m_rep`.
- `print()` and `summary()` methods have been revised.
- The arguments `w` (case weights) and `y` (response) can now also be passed as column *names*.

## Minor changes

- Statistics: The argument `top_m` has been moved to the `plot()` method.
- Statistics: The clipping threshold `eps` of squared numerator statistics has been reduced from `1e-8` to `1e-10`. It is now handled in `hstats()` instead of the statistic functions.
- `H-squared`: The $H^2$ statistic stored in a "hstats" object is now a matrix with one row (it was a vector).
- `pd_importance()`: The "hstats" object now contains pre-calculated PD-based importance values in `$pd_importance`.
- `summary.hstats()` now returns an object of class "hstats_summary" instead of "summary_hstats".
- `average_loss()` is more flexible regarding the group `BY` argument. It can also be a variable *name*. Non-discrete `BY` variables are now automatically binned. Like `partial_dep()`, binning is controlled by the `by_size = 4` argument.
- `average_loss()` also returns a "hstats_matrix" object with `print()` and `plot()` method. The values can be extracted via `$M`.
- The default `v` of `hstats()` and `perm_importance()` is now `NULL`. Internally, it is set to `colnames(X)` (minus the column names of `w` and `y` if passed as name).
- Missing grid values: `partial_dep()` and `ice()` have received a `na.rm` argument that controls if missing values are dropped during grid creation. The default `TRUE` is compatible with earlier releases.
- Missing values in `hstats()`: Discrete variables with missings would cause `rowsum()` to launch repeated warnings. This case is now catched.
- The position of some function arguments have changed.
- `perm_importance()`: The default of `verbose` is `TRUE` again.

# hstats 0.3.0

This is intended to be the last version before 1.0.0.

## Visible changes

- Grid of `ice()` and `partial_dep()`: So far, the default grid strategy "uniform" used `pretty()` to generate the evaluation points. To provide more predictable grid sizes, and to be more in line with other implementations of partial dependence and ICE, we now use `seq()` to create the uniform grid.
- `h2_pairwise()` and `h2_threeway()` will now also include 0 values. Use `zero = FALSE` to drop them, see below. The padding with 0 is done at no computational cost, and will affect only up to `pairwise_m` and `threeway_m` features.
- The `print()` method of `summary.hstats()` is less verbose.

## Improvements

- `h2_overall()`, `h2_pairwise()`, `h2_threeway()`, `plot.hstats()`, and `summary.hstats()` have received an argument `zero = TRUE`. Set to `FALSE` to drop statistics having value 0.
- `perm_importance()` and `average_loss()` will now recycle a univariate response when combined with multivariate predictions. This is useful, e.g., when the prediction function represents the predictions of multiple models that should be evaluated against a common response.

## Bug fixes

- All progress bars were initialized 1 step too late.
- `perm_importance()` and `average_loss()` would fail for "mlogloss" in case the response `y` was univariate *and* non-factor/non-character.

## Other changes

- All available H-statistics are now calculated within `hstats()` and attached to the resulting object. Each statistic is stored as list with numerator and denominator matrices/vectors. The functions `h2()`, `h2_overall()`, `h2_pairwise()`, and `h2_threeway()`, `print.hstats()`, `summary().hstats()`, `plot.hstats()` will use these without having to recalculate the required numerators and denominators. The results, however, are unchanged.

# hstats 0.2.0

## New major features

- **average_loss()**: This new function calculates the average loss of a model for a given dataset, optionally grouped by a discrete vector. It supports the most important loss functions (squared error, Poisson deviance, Gamma deviance, Log loss, multivariate Log loss, absolute error, classification error), and allows for case weights. Custom losses can be passed as vector/matrix valued functions of signature `f(obs, pred)`.
Note that such a custom function needs to return per-row losses, not their average.

- **perm_importance()**: H-statistics are often calculated for important features only. To support this workflow, we have added permutation importance regarding the most important loss functions. Multivariate losses can be studied individually or collapsed over dimensions. The importance of *feature groups* can be studied as well. Note that the API of `perm_importance()` is different from the experimental `pd_importance()`, which is calculated from a "hstats" object.

## Major changes in defaults

- `hstats()` now uses the default feature vector `v = colnames(X)`, simplifying the API in most cases. The typical call is now `hstats(object, X = Feature data)`. 
- `h2_overall()`, `h2_pairwise()`, `h2_threeway()`, `pd_importance()` by default do not plot results anymore. Set `plot = TRUE` to do so.

## Minor changes

- `summary.hstats()` now returns an object of class "summary_hstats" with its own `print()` method. Like this, one can use `su <- summary()` without printing to the console.
- The output of `summary.hstats()` is printed slightly more compact.
- `plot.hstats()` has recieved a `rotate_x = FALSE` argument for rotating x labels by 45 degrees.
- `plot.hstats()` and `summary.hstats()` have received explicit arguments `normalize`, `squared`, `sort`, `eps` instead of passing them via `...`.
- `plot.hstats()` now passes `...` to `geom_bar()`.
- Slight speed-up of `hstats()` in the one-dimensional case.

## Bug fixes

- Probabilistic {mlr3} classifiers did not work out-of-the box. This has been fixed.

# hstats 0.1.0

This is the initial release.
