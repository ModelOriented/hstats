#' Fast Partial Dependence Function
#' 
#' @description
#' Fast implementation of Friedman's (empirical) partial dependence (PD) function 
#' of feature subset \eqn{J}, given by
#' \deqn{\textrm{PD}_J(v) = \frac{1}{|D|} \sum_{i \in D} \hat f(v, x_{i,\setminus J})},
#' where
#' - \eqn{D} is a reference data set,
#' - \eqn{J} is a index set
#' - \eqn{\hat f} is the fitted model, and
#' - \eqn{x_{i,\setminus J}} is the feature vector of the i-th observation without 
#'   components in \eqn{J} (which are replaced by the function argument(s) \eqn{v}).
#' 
#' The function supports both 
#' - multivariate predictions (e.g., multi-classification settings) and
#' - multivariate grids.
#' 
#' @param object Fitted model object.
#' @param v Vector of feature names.
#' @param X Dataframe or matrix serving as background dataset.
#' @param pred_fun Prediction function of the form `function(object, X, ...)`,
#'   providing K >= 1 numeric predictions per row. Its first argument represents the 
#'   model `object`, its second argument a data structure like `X`. Additional arguments 
#'   (such as `type = "response"` in a GLM) can be passed via `...`. The default, 
#'   [stats::predict()], will work in most cases. Note that column names in a resulting
#'   matrix of predictions will be used as default column names in the results.
#' @param grid Named list. Each element specifies the evaluation grid for the
#'   corresponding feature. Missing components are automatically added. If `v` has
#'   length 1, then `grid` can also be a vector.
#' @param grid_size Controls the grid size for variables not in `grid`.
#' @param trim Variables not in `grid` are trimmed at those two quantiles.
#'   Set to `c(0, 1)` for no trimming.
#' @param strategy How should evaluation points of variables not in `grid` be found? 
#'   Either "quantile" or "uniform".
#' @param n_max If `X` has more than `n_max` rows, a random sample of `n_max` rows is
#'   selected for calculations (after determining `grid`). 
#' @param w Optional vector of case weights for each row of `X`.
#' @param verbose Should a progress bar be shown? The default is `TRUE`.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`.
#' @returns 
#'   An object of class "fx_pd", containing these elements:
#'   - `grid`: Named list of evaluation points.
#'   - `pd`: Named list of PD matrices.
#'   - `v`: Same as input `v`.
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'     The Annals of Statistics, 29:1189–1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' pd <- fx_pd(fit, v = names(iris[-1]), X = iris)
#' pd
#' head(summary(pd))
#' summary(pd, "Species")
#' 
#' pd <- fx_pd(fit, v = "Petal.Width", X = iris, grid = seq(0, 1, by = 0.5))
#' summary(pd)
#' summary(fx_pd(fit, v = "Petal.Width", X = iris, grid = seq(1, 0, by = -0.5)))
#' summary(fx_pd(fit, v = "Species", X = iris))
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- names(iris[3:5])
#' partial_grid <- list(Petal.Width = seq(0, 1, by = 0.5))
#' pd <- fx_pd(fit, v = v, X = iris, grid = partial_grid, verbose = FALSE)
#' summary(pd, "Species")
#' summary(pd, "Petal.Width")
#' head(summary(pd, "Petal.Length"))
#' 
#' # MODEL THREE: Gamma GLM with log link
#' fit <- glm(
#'   Sepal.Length ~ . + Petal.Width:Species, 
#'   data = iris, 
#'   family = Gamma(link = log)
#' )
#' summary(fx_pd(fit, v = "Species", X = iris, type = "response"))
fx_pd <- function(object, ...) {
  UseMethod("fx_pd")
}

#' @describeIn fx_pd Default method.
#' @export
fx_pd.default <- function(object, v, X, pred_fun = stats::predict, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           strategy = c("quantile", "uniform"), n_max = 1000L, 
                           w = NULL, verbose = TRUE, ...) {
  strategy <- match.arg(strategy)
  p <- length(v)
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= c(1L, 1L),
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X)
  )
  
  # Make list of grid values per v
  if ((p == 1L) && (is.vector(grid) || is.factor(grid))) {
    grid <- stats::setNames(list(grid), v)
  } else {
    for (z in v) {
      if (is.null(grid[[z]])) {
        zz <- if (is.data.frame(X)) X[[z]] else X[, z]
        grid[[z]] <- make_grid_one(zz, m = grid_size, trim = trim, strategy = strategy)
      }
    }
  }

  # Reduce size of X (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
  }
  
  # Initialize progress bar
  show_bar <- verbose && p >= 2L
  if (show_bar) {
    j <- 1L
    pb <- utils::txtProgressBar(1L, p, style = 3)
  }
  
  # Calculations
  pd <- stats::setNames(vector("list", length = p), v)
  for (z in v) {
    pd[[z]] <- pdp_raw(
      object = object, 
      v = z, 
      X = X, 
      pred_fun = pred_fun, 
      grid = grid[[z]], 
      w = w,
      compress_grid = FALSE,  # Almost always unique, so we save a check for uniqueness
      ...
    )
    if (show_bar) {
      utils::setTxtProgressBar(pb, j)
      j <- j + 1L
    }
  }
  if (show_bar) {
    cat("\n")
  }
  structure(list(grid = grid, pd = pd, v = v), class = "fx_pd")
}

#' @describeIn fx_pd Method for "ranger" models.
#' @export
fx_pd.ranger <- function(object, v, X, 
                          pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                          grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                          strategy = c("quantile", "uniform"), n_max = 1000L,
                          w = NULL, verbose = TRUE, ...) {
  fx_pd.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' @describeIn fx_pd Method for "mlr3" models.
#' @export
fx_pd.Learner <- function(object, v, X, 
                           pred_fun = function(m, X) m$predict_newdata(X)$response, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99),
                           strategy = c("quantile", "uniform"), n_max = 1000L, 
                           w = NULL, verbose = TRUE, ...) {
  fx_pd.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' PD Print
#' 
#' Print function for result of [fx_pd()].
#' 
#' @param x Object to print.
#' @param ... Currently unused.
#' @returns Invisibly, `x` is returned.
#' @export
#' @seealso [fx_pd()]
print.fx_pd <- function(x, ...) {
  cat("PD values. Use summary(pd, variable) to extract the results.")
  invisible(x)
}

#' PD Summary
#' 
#' Uses the results of [fx_pd()] to show PD values for a given variable.
#' 
#' @param object Object to summarize.
#' @param which Name or position (within `v`) of feature to show partial dependence.
#' @param out_names Optional names of the output columns corresponding to the 
#'   K-dimensional predictions.
#' @param ... Currently unused.
#' @returns data.frame with evaluation grid and PD values (one column per prediction).
#' @export
#' @seealso [fx_pd()]
summary.fx_pd <- function(object, which = 1L, out_names = NULL, ...) {
  cbind.data.frame(
    object[["grid"]][which], 
    fix_names(object[["pd"]][[which]], out_names = out_names)
  )
}

#' Barebone Partial Dependence Function
#' 
#' @description
#' Creates partial dependence values for a given model, data, and grid. 
#' This is the workhorse function behind [fx_pd()], [fx_interaction()], 
#' and [fx_importance()].
#' 
#' It is fast because:
#' 1. There is a single call to `pred_fun()`.
#' 2. If more than 5% of grid rows are duplicated, they are dropped from the 
#'   calculation. Resulting PD values are mapped back. This is important for 
#'   [fx_interaction()] and [fx_importance()] that use sampled data as grid.
#' 3. If more than 5% of non-grid rows are duplicated, these are dropped and
#'   compensated by summing up their case weights.
#' 
#' @inheritParams fx_pd
#' @param v Vector of variable name(s) to calculate partial dependence for.
#' @param grid A vector (if `length(v) == 1L`), or a matrix/data.frame otherwise.
#' @param compress_X If `X` has a single non-`v` column: should duplicates be removed
#'   and compensated via case weights? (Applied only if >5% duplicates.)
#'   Default is `TRUE`.
#' @param compress_grid Should duplicates in `grid` be removed, and resulting PDs 
#'   mapped back to the original grid index? (Applied only if >5% duplicates.)
#'   Default is `TRUE`.
#' @returns 
#'   A matrix of partial dependence values (one column per prediction dimension, 
#'   one row per grid row).
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'   The Annals of Statistics, 29:1189–1232.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' pdp_raw(fit, v = "Petal.Width", X = iris, pred_fun = predict, grid = 1:2)
pdp_raw <- function(object, v, X, pred_fun, grid, w = NULL, 
                    compress_X = TRUE, compress_grid = TRUE, ...) {
  p <- length(v)
  D1 <- p == 1L
  if (compress_X && p >= ncol(X) - 1L) {
    # Removes duplicates in X[, not_v] and compensates via w
    cmp_X <- .compress_X(X = X, v = v, w = w)
    X <- cmp_X[["X"]]
    w <- cmp_X[["w"]]
  }
  
  if (compress_grid) {
    # Removes duplicates in grid and returns reindex vector to match to original grid
    cmp_grid <- .compress_grid(grid = grid, v = v)
    grid <- cmp_grid[["grid"]]
  }
  
  n <- nrow(X)
  n_grid <- NROW(grid)
  
  # Explode everything to n * n_grid rows
  X_pred <- X[rep(seq_len(n), times = n_grid), , drop = FALSE]
  if (D1) {
    grid_pred <- rep(grid, each = n)
  } else {
    grid_pred <- grid[rep(seq_len(n_grid), each = n), ]
  }
  
  # Vary v
  if (D1 && is.data.frame(X_pred)) {
    X_pred[[v]] <- grid_pred  #  [, v] <- slower if df
  } else {
    X_pred[, v] <- grid_pred
  }
  
  pred <- check_pred(pred_fun(object, X_pred, ...))
  pd <- rowmean(pred, ngroups = n_grid, w = w)
  if (compress_grid && !is.null(reindex <- cmp_grid[["reindex"]])) {
    return(pd[reindex, , drop = FALSE])
  }
  pd
}

