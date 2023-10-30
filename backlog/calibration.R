#' Calibration Plot "Plus"
#' 
#' Calculates average observed, average predicted, and partial dependence of a single
#' feature `v` over its binned range. 
#' 
#' @inheritParams hstats
#' @param v One or more column names over which you want to calculate the partial
#'   dependence.
#' @param grid Evaluation grid. A vector (if `length(v) == 1L`), or a matrix/data.frame 
#'   otherwise. If `NULL`, calculated via [multivariate_grid()].
#' @param BY Optional grouping vector or column name. The partial dependence
#'   function is calculated per `BY` group. Each `BY` group
#'   uses the same evaluation grid to improve assessment of (non-)additivity.
#'   Numeric `BY` variables with more than `by_size` disjoint values will be 
#'   binned into `by_size` quantile groups of similar size. To improve robustness,
#'   subsampling of `X` is done within group. This only applies to `BY` groups with
#'   more than `n_max` rows.
#' @param by_size Numeric `BY` variables with more than `by_size` unique values will
#'   be binned into quantile groups. Only relevant if `BY` is not `NULL`.
#' @returns 
#'   An object of class "calibration" containing these elements:
#'   - `data`: data.frame containing the partial dependencies.
#'   - `v`: Same as input `v`.
#'   - `K`: Number of columns of prediction matrix.
#'   - `pred_names`: Column names of prediction matrix.
#'   - `by_name`: Column name of grouping variable (or `NULL`).
#' @references
#'   Friedman, Jerome H. *"Greedy Function Approximation: A Gradient Boosting Machine."* 
#'     Annals of Statistics 29, no. 5 (2001): 1189-1232.
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' (calib <- calibration(fit, v = "Petal.Length", X = iris, y = "Sepal.Length"))
#' plot(calib)
#' 
#' (calib <- calibration(fit, v = "Petal.Length", X = iris, y = "Sepal.Length", BY = "Species"))
#' plot(calib)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' calib <- calibration(fit, v = "Petal.Width", X = iris, y = iris[1:2])
#' plot(calib, show_points = FALSE)
#' 
#' # Multivariate, multivariable, and BY (no plot available)
#' pd <- calibration(
#'   fit, v = c("Petal.Width", "Petal.Length"), X = iris, BY = "Species"
#' )
#' pd
#' 
#' # MODEL 3: Gamma GLM -> pass options to predict() via ...
#' fit <- glm(Sepal.Length ~ ., data = iris, family = Gamma(link = log))
#' plot(calibration(fit, v = "Petal.Length", X = iris), show_points = FALSE)
#' plot(calibration(fit, v = "Petal.Length", X = iris, type = "response"))
calibration <- function(object, ...) {
  UseMethod("calibration")
}

#' @describeIn calibration Default method.
#' @export
calibration.default <- function(object, v, X, y = NULL, pred_fun = stats::predict, 
                                BY = NULL, by_size = 4L, 
                                grid_size = 17L, 
                                pred = NULL,
                                n_max = 1000L, w = NULL, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    is.function(pred_fun),
    v %in% colnames(X)
  )
  
  if (!is.null(y)) {
    y <- prepare_y(y = y, X = X)[["y"]]
    if (is.factor(y) || is.character(y)) {
      y <- stats::model.matrix(~ as.factor(y) + 0)
    }
    y <- align_pred(y)
  }
  if (!is.null(w)) {
    w <- prepare_w(w = w, X = X)[["w"]]
  }
  if (!is.null(BY)) {
    BY2 <- prepare_by(BY = BY, X = X, by_size = by_size)
    BY <- BY2[["BY"]]
  }
  g <- v_grouped <- approx_vector(X[[v]], m = grid_size)
  grid <- sort(unique(v_grouped), na.last = TRUE)
  
  if (!is.null(BY)) {
    g <- paste(BY, g, sep = ":")
  }
  
  # Average predicted
  if (is.null(pred)) {
    pred <- pred_fun(object, X, ...)
  }
  pred <- align_pred(pred)
  tmp <- gwColMeans(pred, g = g, w = w, mean_only = FALSE)
  avg_pred <- tmp[["mean"]]
  
  # Exposure
  exposure <- tmp[["denom"]]
  
  # Average observed
  avg_obs <- if (!is.null(y)) gwColMeans(y, g = g, w = w)
  
  # Partial dependence
  pd <- partial_dep(
    object = object, 
    v = v, 
    X = X, 
    grid = grid, 
    pred_fun = pred_fun, 
    BY = BY, 
    w = w, 
    ...
  )[["data"]]
  
  out <- list(
    v = v,
    K = ncol(pred),
    pred_names = colnames(pred),
    grid = grid,
    BY,
    avg_obs = avg_obs,
    avg_pred = avg_pred,
    pd = pd,
    exposure = exposure
  )
  return(structure(out, class = "calibration"))
}

#' @describeIn calibration Method for "ranger" models.
#' @export
calibration.ranger <- function(object, v, X, 
                               pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                               BY = NULL, by_size = 4L, grid = NULL, grid_size = 49L, 
                               trim = c(0.01, 0.99), 
                               strategy = c("uniform", "quantile"), na.rm = TRUE,
                               n_max = 1000L, w = NULL, ...) {
  calibration.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    BY = BY,
    by_size = by_size,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    na.rm = na.rm,
    n_max = n_max,
    w = w,
    ...
  )
}

#' @describeIn calibration Method for "mlr3" models.
#' @export
calibration.Learner <- function(object, v, X, 
                                pred_fun = NULL,
                                BY = NULL, by_size = 4L, grid = NULL, grid_size = 49L, 
                                trim = c(0.01, 0.99), 
                                strategy = c("uniform", "quantile"), na.rm = TRUE,
                                n_max = 1000L, w = NULL, ...) {
  if (is.null(pred_fun)) {
    pred_fun <- mlr3_pred_fun(object, X = X)
  }
  calibration.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    BY = BY,
    by_size = by_size,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    na.rm = na.rm,
    n_max = n_max,
    w = w,
    ...
  )
}

#' @describeIn calibration Method for DALEX "explainer".
#' @export
calibration.explainer <- function(object, v, X = object[["data"]], 
                                  pred_fun = object[["predict_function"]],
                                  BY = NULL, by_size = 4L, grid = NULL, grid_size = 49L, 
                                  trim = c(0.01, 0.99), 
                                  strategy = c("uniform", "quantile"), na.rm = TRUE,
                                  n_max = 1000L, w = object[["weights"]], ...) {
  calibration.default(
    object = object[["model"]],
    v = v,
    X = X,
    pred_fun = pred_fun,
    BY = BY,
    by_size = by_size,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    na.rm = na.rm,
    n_max = n_max,
    w = w,
    ...
  )
}

#' Prints "calibration" Object
#' 
#' Print method for object of class "calibration".
#'
#' @param x An object of class "calibration".
#' @param n Number of rows to print.
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [calibration()] for examples.
print.calibration <- function(x, n = 3L, ...) {
  cat("Calibration object. Top rows:\n", sep = "")
  for (what in c("avg_obs", "avg_pred", "pd", "exposure")) {
    if (!is.null(x[[what]])) {
      cat("\n", what, "\n")
      print(utils::head(x[[what]], n))
    }
  }
  invisible(x)
}

#' Plots "calibration" Object
#' 
#' Plot method for objects of class "calibration".
#' 
#' @param x An object of class "calibration".
#' @inheritParams plot.partial_dep
#' @export
#' @returns An object of class "ggplot".
#' @seealso See [calibration()] for examples.
plot.calibration <- function(x,
                             color = getOption("hstats.color"),
                             viridis_args = getOption("hstats.viridis_args"),
                             facet_scales = "fixed",
                             rotate_x = FALSE, show_points = TRUE, 
                             ...) {
  if (is.null(viridis_args)) {
    viridis_args <- list()
  }
  
  dat <- list(
    Obs = mat2df(x$avg_obs, id = "Obs"),
    Pred = mat2df(x$avg_pred, id = "Pred"),
    PD = mat2df(x$pd, id = "PD")
  )
  dat <- do.call(rbind, dat)
  dat <- transform(
    dat, 
    variable_ = x$grid,
    id_ = factor(id_, c("Obs", "Pred", "PD"))
  )
  
  p <- ggplot2::ggplot(
    dat, ggplot2::aes(x = variable_, y = value_, group = id_, color = id_)
  ) +
    ggplot2::geom_line(...) +
    do.call(ggplot2::scale_color_viridis_d, viridis_args) +
    ggplot2::labs(x = x[["v"]], y = "Prediction scale") +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  if (show_points) {
    p <- p + ggplot2::geom_point()
  }
  if (x$K > 1L) {
    p <- p + ggplot2::facet_wrap("varying_", scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + rotate_x_labs()
  }
  p
}

#' Histogram Bin Construction
#' 
#' Creates histogram of vector/factor `x`. In the discrete case, no binning is done.
#' Otherwise, the values are optionally trimmed and then passed to [hist()]. Compared
#' with [hist()], the function also returns the binned values of `x`.
#' 
#' @param x A vector or factor to be binned.
#' @inheritParams hist
#' @inheritParams univariate_grid
#' @returns A list with binned "x", vector of "breaks", bin midpoints "grid", and a
#'   logical flag "discrete" indicating whether the values have not been binned.
#' @seealso See [calibration()] for examples.
hist2 <- function(x, breaks = 17L, trim = c(0.01, 0.99), 
                  include.lowest = TRUE, right = TRUE, na.rm = TRUE) {
  g <- unique(x)
  if (!is.numeric(x) || (length(breaks) == 1L && is.numeric(breaks) && length(g) <= breaks)) {
    g <- sort(g, na.last = if (na.rm) NA else TRUE)
    return(list(x = x, breaks = g, grid = g, discrete = TRUE))
  }
  
  # Trim outliers before histogram construction?
  if (trim[1L] == 0 && trim[2L] == 1) {
    xx <- x
  } else {
    r <- stats::quantile(x, probs = trim, names = FALSE, type = 1L, na.rm = TRUE)
    xx <- x[x >= r[1L] & x <= r[2L]]
  }
  h <- hist(
    xx, breaks = breaks, include.lowest = include.lowest, right = right, plot = FALSE
  )
  b <- h$breaks
  ix <- findInterval(
    x, vec = b, left.open = right, rightmost.closed = include.lowest, all.inside = TRUE
  )
  g <- h$mids
  if (!na.rm && anyNA(x)) {
    g <- c(g, NA)
  }
  list(x = g[ix], breaks = b, grid = g, discrete = FALSE)
}
