#' Fast PDP
#' 
#' @param object Fitted model object.
#' @param v One or more variable names in `X` to calculate partial dependence profiles.
#' @param X Dataframe or matrix serving as background dataset.
#' @param pred_fun Prediction function of the form `function(object, X, ...)`,
#'   providing \eqn{K \ge 1} numeric predictions per row. Its first argument 
#'   represents the model `object`, its second argument a data structure like `X`. 
#'   Additional (named) arguments are passed via `...`. 
#'   The default, [stats::predict()], will work in most cases. 
#' @param grid Optional evaluation grid. If `v` is a single column name, this is a 
#'   vector/factor. Otherwise, a matrix or `data.frame` with `length(v)` columns.
#' @param grid_size Determines the grid When `grid = NULL`. Character/factor variables
#'   are evaluated at each unique value. A numeric `v` with more than `grid_size` unique
#'   values is evaluated at `grid_size` quantiles. If `v` has length \eqn{p > 1},
#'   the \eqn{p}th root of `grid_size` is used instead. 
#' @param trim A vector with two probabilities used to trim non-discrete numeric `v` 
#'   before applying quantile binning (only if `grid = NULL`). 
#'   Set to `c(0, 1)` to avoid trimming.
#' @param n_max If `X` has more rows than `n_max`, a random sample of `n_max` rows is
#'   selected. 
#' @param out_names Names of the output columns.
#' @param w Optional vector of case weights for each row of `X`.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`.
#' @returns A dataframe with partial dependence per grid value.
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'   The Annals of Statistics, 29:1189–1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' pd <- fx_pdp(fit, v = "Petal.Width", X = iris)
#' pd[1:4, ]
#' 
#' fx_pdp(fit, v = "Petal.Width", X = iris, grid = seq(0, 1, by = 0.5), out_name = "P")
#' fx_pdp(fit, v = "Petal.Width", X = iris, grid = seq(1, 0, by = -0.5))
#' fx_pdp(fit, v = "Species", X = iris)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' fx_pdp(fit, v = "Species", X = iris)
#' pd <- fx_pdp(fit, v = c("Petal.Width", "Species"), X = iris)
#' pd[1:4, ]
#' 
#' # MODEL THREE: Gamma GLM with log link
#' fit <- glm(Sepal.Length ~ Species + Petal.Width, data = iris, family = Gamma(link = log))
#' fx_pdp(fit, v = "Petal.Width", X = iris, type = "response")
#' 
fx_pdp <- function(object, ...) {
  UseMethod("fx_pdp")
}

#' @describeIn fx_pdp Default method.
#' @export
fx_pdp.default <- function(object, v, X, pred_fun = stats::predict, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           n_max = 1000L, out_names = NULL, w = NULL, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= 2:1,
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X)
  )
  
  # Reduce size of X (and w)
  n <- nrow(X)
  if (n > n_max) {
    ix <- sample(n, n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
    n <- n_max
  }

  # Make/check grid. If length(v) == 1, grid is always a vector/factor
  if (is.null(grid)) {
    grid <- fixed_grid(X[, v], m = grid_size, trim = trim)
  } else{
    check_grid(grid, v = v, X_is_matrix = is.matrix(X))
  }
  
  # Calculations
  pd <- pdp_raw(
    object = object, v = v, X = X, pred_fun = pred_fun, grid = grid, w = w, ...
  )
  
  # Cleanup
  pd <- fix_names(pd, out_names = out_names)
  if (!is.data.frame(grid)) {
    grid <- stats::setNames(as.data.frame(grid), v)
  }
  cbind.data.frame(grid, pd)
}


#' @describeIn fx_pdp Method for "ranger" models, see Readme for an example.
#' @export
fx_pdp.ranger <- function(object, v, X, 
                          pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                          grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                          n_max = 1000L, out_names = NULL, w = NULL, ...) {
  fx_pdp.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    n_max = n_max,
    out_names = out_names,
    w = w,
    ...
  )
}

#' @describeIn fx_pdp Method for "mlr3" models, see Readme for an example.
#' @export
fx_pdp.Learner <- function(object, v, X, 
                           pred_fun = function(m, X) m$predict_newdata(X)$response, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           n_max = 1000L, out_names = NULL, w = NULL, ...) {
  fx_pdp.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    n_max = n_max,
    out_names = out_names,
    w = w,
    ...
  )
}

# Barebone function to calculate PD values. Arguments see fx_pdp()
# If length(v) == 1, then grid must be a vector/factor.
# Output is matrix of partial dependence values in the same order as grid
pdp_raw <- function(object, v, X, pred_fun, grid, w = NULL, ...) {
  # Optimize case where X[, not v] is one-dimensional with >10% duplicates
  # This is useful in Friedman's H (overall interaction strength per feature)
  # Implemented by summing up w of duplicated X[, not v]
  not_v <- setdiff(colnames(X), v)
  if (length(not_v) == 1L) {
    x_not_v <- if (is.data.frame(X)) X[[not_v]] else X[, not_v]
    X_dup <- duplicated(x_not_v)
    if (mean(X_dup) > 0.1) {
      if (is.null(w)) {
        w <- rep(1.0, times = nrow(X))
      }
      X <- X[!X_dup, , drop = FALSE]
      w <- c(rowsum(w, g = x_not_v, reorder = FALSE))
    }
  }
  
  n <- nrow(X)
  D1 <- length(v) == 1L
  
  # Duplicated values of grid can be removed but, we need to map the PD values
  # back to the original grid position
  ugrid <- unique(grid)
  if (NROW(ugrid) < 0.9 * NROW(grid)) {
    is_dup <- TRUE
    if (D1) {
      orig <- grid  # always a vector/factor
      final <- ugrid
    } else {
      orig <- apply(grid, MARGIN = 1L, FUN = paste, collapse = "_:_")
      final <- apply(ugrid, MARGIN = 1L, FUN = paste, collapse = "_:_")
      if (anyDuplicated(final)) {
        stop("String '_:_' found in grid values at unlucky position.")
      }
    }
    grid <- ugrid
  } else{
    is_dup <- FALSE
  }
  
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
    X_pred[[v]] <- grid_pred  #  [, v] <- much slower if df
  } else {
    X_pred[, v] <- grid_pred
  }
  
  pred <- check_pred(pred_fun(object, X_pred, ...))
  pd <- rowmean(pred, ngroups = n_grid, w = w)
  rownames(pd) <- NULL
  if (is_dup) {
    return(pd[match(orig, final), , drop = FALSE])
  }
  pd
}

