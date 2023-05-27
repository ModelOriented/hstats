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
#' @param w Optional vector of case weights for each row of `X`.
#' @param grid Optional evaluation grid. If `v` is a single column name, this is a 
#'   vector. Otherwise, a matrix or `data.frame` with `length(v)` columns.
#' @param grid_size This is the number of rows sampled randomly from `X[, v]`. 
#'   The result serves as evaluation `grid`. Only used if `grid = NULL`.
#' @param trim When `grid = NULL` and `grid_type = "fixed"`, numeric columns are
#'   trimmed first at those quantiles. Set to `c(0, 1)` to avoid trimming.
#' @param pred_n description
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`.
#' @returns 
#'   An object of class "fx_friedmans_h" containing
#'   - `pd`: A vector, matrix or `data.frame` with PD values. The row order corresponds
#'     to the `grid` attached to the output.
#'   - `grid`: A vector, matrix, or `data.frame` representing the evaluation grid in
#'     the same row order as `pd`. Note that this is a sorted and deduplicated version
#'     of the input `grid` (if provided).
#'   - `replications`: How many times each value/row in the output `grid` was present
#'     in the input `grid`. Like a case weight of the result.
#'   - `v`: The input `v`.
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'   The Annals of Statistics, 29:1189–1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ Species * Petal.Width, data = iris)
#' fx_friedmans_h.default(fit, v = c("Petal.Width", "Species"), X = iris)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' h <- fx_friedmans_h(fit, v = colnames(iris[2:5]), X = iris)
#' h[1:3, ]
#' h <- fx_friedmans_h(fit, v = c("Petal.Width", "Species"), X = iris)
#' h
fx_friedmans_h <- function(object, ...) {
  UseMethod("fx_friedmans_h")
}

#' @describeIn fx_friedmans_h Default method.
#' @export
fx_friedmans_h.default <- function(object, v, X, pred_fun = stats::predict, 
                                   normalize = TRUE, take_sqrt = TRUE,
                                   grid_size = 50L, n_max = 200L, pd_names = NULL, 
                                   w = NULL, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= c(2L, 2L),
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X),
    length(v) >= 2L
  )
  
  n <- nrow(X)
  
  # Grid with all v
  grid <- if (n > grid_size) X[sample(n, grid_size), v] else X[, v]
  if (!is.data.frame(grid)) {
    grid <- as.data.frame(grid)
  }

  # Reduce size of X (and w)
  if (n > n_max) {
    ix <- sample(n, n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
    n <- n_max
  }
  
  # First, create univariate results
  friedman_1d <- function(z, grid) {
    pd <- pdp_raw(
      object, v = z, X = X, pred_fun = pred_fun, grid = unique(grid[[z]]), w = w, ...
    )
    .scale(pd$pd[match(grid[[z]], pd$grid[[z]]), , drop = FALSE])
  }
  pd1d <- setNames(lapply(v, FUN = friedman_1d, grid = grid), v)
  
  # Then, loop over bivariate cases
  all_combs <- utils::combn(v, 2L, simplify = FALSE)
  ncomb <- length(all_combs)
  out <- vector("list", length = ncomb)
  if (ncomb > 1L) {
    pb <- utils::txtProgressBar(1L, ncomb, style = 3)
  }
  
  for (i in seq_len(ncomb)) {
    z <- all_combs[[i]]
    pd <- pdp_raw(
      object, v = z, X = X, pred_fun = pred_fun, grid = unique(grid[, z]), w = w, ...
    )
    orig <- interaction(grid[, z], drop = TRUE, sep = ":_:")
    post <- interaction(pd$grid, drop = TRUE, sep = ":_:")
    if (anyDuplicated(post)) {  # Extremly improbable
      stop("non-unique combinations v1:_:v2")
    }
    pd <- .scale(pd$pd[match(orig, post), ])
    H <- colMeans((pd - pd1d[[z[1L]]] - pd1d[[z[2L]]])^2)
    if (normalize) {
      H <- H / colMeans(pd^2)
    }
    if (take_sqrt) {
      H <- sqrt(H)
    }
    H <- fix_pd_names(t(.zap_small(H)), pd_names = pd_names)
    
    if (ncomb > 1L) {
      utils::setTxtProgressBar(pb, i)
    }
    out[[i]] <- cbind.data.frame(V1 = z[1L], V2 = z[2L], H)
  }
  do.call(rbind, out)
}


#' @describeIn fx_friedmans_h Method for "ranger" models, see Readme for an example.
#' @export
fx_friedmans_h.ranger <- function(object, v, X, 
                           pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           n_max = 500L, pd_names = NULL, w = NULL, ...) {
  fx_friedmans_h.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    n_max = n_max,
    pd_names = pd_names,
    w = w,
    ...
  )
}

#' @describeIn fx_friedmans_h Method for "mlr3" models, see Readme for an example.
#' @export
fx_friedmans_h.Learner <- function(object, v, X, 
                            pred_fun = function(m, X) m$predict_newdata(X)$response, 
                            grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                            n_max = 500L, pd_names = NULL, w = NULL, ...) {
  fx_friedmans_h.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    n_max = n_max,
    pd_names = pd_names,
    w = w,
    ...
  )
}


# Helper

# Helper function used to clip small values.
.zap_small <- function(x, eps = 1e-8) {
  zero <- abs(x) < eps | !is.finite(x)
  if (any(zero)) {
    x[zero] <- 0
  }
  x
}

.scale <- function(x) {
  sweep(x, MARGIN = 2L, STATS = colMeans(x))
}
