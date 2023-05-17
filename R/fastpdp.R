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
#'   The default (`NULL`) will randomly sample rows from `X[, v]`.
#' @param grid_type Type of grid. If "fixed", will use unique values for discrete
#'   features, and quantile cuts otherwise.
#' @param grid_size This is the number of rows sampled randomly from `X[, v]`. 
#'   The result serves as evaluation `grid`. Only used if `grid = NULL`.
#' @param trim When `grid = NULL` and `grid_type = "fixed"`, numeric columns are
#'   trimmed first at those quantiles. Set to `c(0, 1)` to avoid trimming.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`.
#' @returns 
#'   An object of class "fastpdp" containing
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
#' fastpdp(fit, v = "Petal.Width", X = iris)
#' fastpdp(fit, v = "Sepal.Width", X = iris, grid = seq(0, 2.5, by = 0.1))
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' fastpdp(fit, v = "Petal.Width", X = iris)
#' fastpdp(fit, v = c("Petal.Width", "Species"), X = iris, grid_type = "random")
fastpdp <- function(object, ...) {
  UseMethod("fastpdp")
}

#' @describeIn fastpdp Default Kernel SHAP method.
#' @export
fastpdp.default <- function(object, v, X, pred_fun = stats::predict, 
                            w = NULL, grid = NULL, grid_type = c("fixed", "random"),
                            grid_size = 27L, trim = c(0.01, 0.99), ...) {
  grid_type <- match.arg(grid_type)
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= 1L,
    all(v %in% colnames(X)),
    is.function(pred_fun)
  )
  
  n <- nrow(X)

  if (is.null(grid)) {
    grid <- make_grid(X[, v], grid_type = grid_type, m = grid_size, trim = trim)
  }
  RLE <- rle2(grid)
  grid <- RLE$values
  
  # Explode X, grid, w to m_grid * nrow(X) rows -> only one call to predict()
  m_grid <- NROW(grid)
  X_pred <- X[rep(seq_len(n), times = m_grid), , drop = FALSE]
  if (length(v) == 1L) {
    g <- rep(grid, each = n)
  } else {
    g <- grid[rep(seq_len(m_grid), each = n), ]
  }
  if (!is.null(w)) {
    if (length(w) != n) {
      stop("'w' needs to be of same length as 'X'")
    }
    w <- rep(w, times = m_grid)
  }
  
  # Vary v
  if (is.data.frame(X) && (length(v) == 1L)) {
    X_pred[[v]] <- g
  } else {
    X_pred[, v] <- g
  }
  
  # Aggregate predictions and organize output
  structure(
    list(
      pd = unname(collapse::fmean(pred_fun(object, X_pred), g = g, w = w)),
      grid = grid,
      replications = RLE$lengths,
      v = v
    ),
    class = "fastpdp"
  )
}


#' @describeIn fastpdp PD method for "ranger" models, see Readme for an example.
#' @export
fastpdp.ranger <- function(object, v, X, 
                           pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
                           w = NULL, grid = NULL, grid_type = c("fixed", "random"),
                           grid_size = 27L, trim = c(0.01, 0.99), ...) {
  fastpdp.default(
    object = object, 
    v = v,
    X = X,
    pred_fun = pred_fun,
    w = w,
    grid = grid,
    grid_type = grid_type,
    grid_size = grid_size,
    trim = trim,
    ...
  )
}

#' @describeIn fastpdp PD method for "mlr3" models, see Readme for an example.
#' @export
fastpdp.Learner <- function(object, v, X, 
                            pred_fun = function(m, X) m$predict_newdata(X)$response, 
                            w = NULL, grid = NULL, grid_type = c("fixed", "random"),
                            grid_size = 27L, trim = c(0.01, 0.99), ...) {
  fastpdp.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    w = w,
    grid = grid,
    grid_type = grid_type,
    grid_size = grid_size,
    trim = trim,
    ...
  )
}
