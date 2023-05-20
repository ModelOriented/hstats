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
#'   An object of class "fx_pdp" containing
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
#' fit <- lm(Sepal.Length ~ Species + Petal.Width, data = iris)
#' pd <- fx_pdp(fit, v = "Petal.Width", X = iris)
#' pd[1:4, ]
#' 
#' fx_pdp(fit, v = "Petal.Width", X = iris, grid = seq(0, 1, by = 0.2), pd_name = "P")
#' fx_pdp(fit, v = "Petal.Width", X = iris, grid = seq(1, 0, by = -0.2))
#' fx_pdp(fit, v = "Species", X = iris)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' pd <- fx_pdp(fit, v = "Species", X = iris)
#' pd[1:3, ]
#' pd <- fx_pdp(fit, v = c("Petal.Width", "Species"), X = iris)
#' pd[1:4, ]
fx_pdp <- function(object, ...) {
  UseMethod("fx_pdp")
}

#' @describeIn fx_pdp Default method.
#' @export
fx_pdp.default <- function(object, v, X, pred_fun = stats::predict, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           n_max = 500L, pd_names = NULL, w = NULL, ...) {
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
  
  # Explode everything to n * n_grid rows and vary v
  n_grid <- NROW(grid)
  X_pred <- X[rep(seq_len(n), times = n_grid), , drop = FALSE]
  if (length(v) == 1L) {
    grid_pred <- rep(grid, each = n)
  } else {
    grid_pred <- grid[rep(seq_len(n_grid), each = n), ]
  }
  if (!is.null(w)) {
    w <- rep(w, times = n_grid)
  }
  X_pred[, v] <- grid_pred
  
  # Create matrix of predictions
  pred <- fix_pred(pred_fun(object, X_pred, ...))
  
  # Turn grid into data.frame to simplify working with collapse
  if (!is.data.frame(grid_pred)) {
    grid_pred <- stats::setNames(as.data.frame(grid_pred), v)
  }
  g <- collapse::GRP(grid_pred, sort = FALSE)
  
  # Calculate PD (always a matrix)
  pd <- collapse::fmean(pred, g = g, w = w)
  rownames(pd) <- NULL
  if (!is.null(pd_names)) {
    colnames(pd) <- pd_names
  } else if (is.null(colnames(pd))) {
    p <- ncol(pd)
    colnames(pd) <- if (p == 1L) "pred" else paste("pred", seq_len(p), sep = "_")
  }
  cbind.data.frame(g[["groups"]], pd)
}


#' @describeIn fx_pdp Method for "ranger" models, see Readme for an example.
#' @export
fx_pdp.ranger <- function(object, v, X, 
                           pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                           grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                           n_max = 500L, pd_names = NULL, w = NULL, ...) {
  fx_pdp.default(
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

#' @describeIn fx_pdp Method for "mlr3" models, see Readme for an example.
#' @export
fx_pdp.Learner <- function(object, v, X, 
                            pred_fun = function(m, X) m$predict_newdata(X)$response, 
                            grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                            n_max = 500L, pd_names = NULL, w = NULL, ...) {
  fx_pdp.default(
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
