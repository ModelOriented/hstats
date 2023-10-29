#' Partial Dependence Plot
#' 
#' Estimates the partial dependence function of feature(s) `v` over a 
#' grid of values. Both multivariate and multivariable situations are supported.
#' The resulting object can be plotted via `plot()`.
#' 
#' @section Partial Dependence Functions: 
#' 
#' Let \eqn{F: R^p \to R} denote the prediction function that maps the 
#' \eqn{p}-dimensional feature vector \eqn{\mathbf{x} = (x_1, \dots, x_p)}
#' to its prediction. Furthermore, let 
#' \deqn{
#'   F_s(\mathbf{x}_s) = E_{\mathbf{x}_{\setminus s}}(F(\mathbf{x}_s, \mathbf{x}_{\setminus s}))
#' }
#' be the partial dependence function of \eqn{F} on the feature subset
#' \eqn{\mathbf{x}_s}, where \eqn{s \subseteq \{1, \dots, p\}}, as introduced in 
#' Friedman (2001). Here, the expectation runs over the joint marginal distribution
#' of features \eqn{\mathbf{x}_{\setminus s}} not in \eqn{\mathbf{x}_s}.
#' 
#' Given data, \eqn{F_s(\mathbf{x}_s)} can be estimated by the empirical partial 
#' dependence function
#' 
#' \deqn{
#'   \hat F_s(\mathbf{x}_s) = \frac{1}{n} \sum_{i = 1}^n F(\mathbf{x}_s, \mathbf{x}_{i\setminus s}),
#' }
#' where \eqn{\mathbf{x}_{i\setminus s}} \eqn{i = 1, \dots, n}, are the observed values
#' of \eqn{\mathbf{x}_{\setminus s}}.
#' 
#' A partial dependence plot (PDP) plots the values of \eqn{\hat F_s(\mathbf{x}_s)}
#' over a grid of evaluation points \eqn{\mathbf{x}_s}.
#' 
#' @inheritParams multivariate_grid
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
#'   An object of class "partial_dep" containing these elements:
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
#' fit <- lm(Sepal.Length ~ . + Species * Petal.Length, data = iris)
#' (pd <- partial_dep(fit, v = "Species", X = iris))
#' plot(pd)
#' 
#' # Stratified by BY variable (numerics are automatically binned)
#' pd <- partial_dep(fit, v = "Species", X = iris, BY = "Petal.Length")
#' plot(pd)
#' 
#' # Multivariable input
#' v <- c("Species", "Petal.Length")
#' pd <- partial_dep(fit, v = v, X = iris, grid_size = 100L)
#' plot(pd, rotate_x = TRUE)
#' plot(pd, d2_geom = "line")  # often better to read
#' 
#' # With grouping
#' pd <- partial_dep(fit, v = v, X = iris, grid_size = 100L, BY = "Petal.Width")
#' plot(pd, rotate_x = TRUE)
#' plot(pd, rotate_x = TRUE, d2_geom = "line")
#' plot(pd, rotate_x = TRUE, d2_geom = "line", swap_dim = TRUE)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[, 1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' pd <- partial_dep(fit, v = "Petal.Width", X = iris, BY = "Species")
#' plot(pd, show_points = FALSE)
#' pd <- partial_dep(fit, v = c("Species", "Petal.Width"), X = iris)
#' plot(pd, rotate_x = TRUE)
#' plot(pd, d2_geom = "line", rotate_x = TRUE)
#' plot(pd, d2_geom = "line", rotate_x = TRUE, swap_dim = TRUE)
#' 
#' # Multivariate, multivariable, and BY (no plot available)
#' pd <- partial_dep(
#'   fit, v = c("Petal.Width", "Petal.Length"), X = iris, BY = "Species"
#' )
#' pd
#' 
#' # MODEL 3: Gamma GLM -> pass options to predict() via ...
#' fit <- glm(Sepal.Length ~ ., data = iris, family = Gamma(link = log))
#' plot(partial_dep(fit, v = "Petal.Length", X = iris), show_points = FALSE)
#' plot(partial_dep(fit, v = "Petal.Length", X = iris, type = "response"))
partial_dep <- function(object, ...) {
  UseMethod("partial_dep")
}

#' @describeIn partial_dep Default method.
#' @export
partial_dep.default <- function(object, v, X, pred_fun = stats::predict, 
                                BY = NULL, by_size = 4L, grid = NULL, grid_size = 49L, 
                                trim = c(0.01, 0.99), 
                                strategy = c("uniform", "quantile"), na.rm = TRUE,
                                n_max = 1000L, w = NULL, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    is.function(pred_fun),
    all(v %in% colnames(X))
  )
  
  # Care about grid
  if (is.null(grid)) {
    grid <- multivariate_grid(
      x = X[, v], grid_size = grid_size, trim = trim, strategy = strategy, na.rm = na.rm
    )
  } else {
    check_grid(g = grid, v = v, X_is_matrix = is.matrix(X))
  }
  
  if (!is.null(w)) {
    w <- prepare_w(w = w, X = X)[["w"]]
  }
  
  # The function itself is called per BY group
  if (!is.null(BY)) {
    BY2 <- prepare_by(BY = BY, X = X, by_size = by_size)
    mm <- length(BY2$by_values)
    pd_list <- vector("list", length = mm)
    for (i in seq_len(mm)) {
      b <- BY2$by_values[i]
      out <- partial_dep.default(
        object = object, 
        v = v, 
        X = X[BY2$BY %in% b, , drop = FALSE],  # works also when by is NA
        pred_fun = pred_fun,
        grid = grid,
        n_max = n_max, 
        w = if (!is.null(w)) w[BY2$BY %in% b],
        ...
      )
      pd_list[[i]] <- out[["data"]]
    }
    pd <- do.call(rbind, c(pd_list, list(make.row.names = FALSE)))
    BY_rep <- rep(BY2$by_values, each = NROW(grid))
    BY_rep <- stats::setNames(as.data.frame(BY_rep), BY2$by_name)
    out[["data"]] <- cbind.data.frame(BY_rep, pd)
    out[["by_name"]] <- BY2$by_name
    
    return(structure(out, class = "partial_dep"))
  }
  
  # Reduce size of X (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
  }

  # Calculations
  pd <- pd_raw(
    object = object, v = v, X = X, grid = grid, pred_fun = pred_fun, w = w, ...
  )
  K <- ncol(pd)
  if (is.null(colnames(pd))) {
    colnames(pd) <- if (K == 1L) "y" else paste0("y", seq_len(K))
  }
  if (!is.data.frame(grid) && !is.matrix(grid)) {
    grid <- stats::setNames(as.data.frame(grid), v)
  }
  
  out <- list(
    data = cbind.data.frame(grid, pd),
    v = v,
    K = K,
    pred_names = colnames(pd),
    by_name = NULL
  )
  return(structure(out, class = "partial_dep"))
}

#' @describeIn partial_dep Method for "ranger" models.
#' @export
partial_dep.ranger <- function(object, v, X, 
                               pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                               BY = NULL, by_size = 4L, grid = NULL, grid_size = 49L, 
                               trim = c(0.01, 0.99), 
                               strategy = c("uniform", "quantile"), na.rm = TRUE,
                               n_max = 1000L, w = NULL, ...) {
  partial_dep.default(
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

#' @describeIn partial_dep Method for "mlr3" models.
#' @export
partial_dep.Learner <- function(object, v, X, 
                                pred_fun = NULL,
                                BY = NULL, by_size = 4L, grid = NULL, grid_size = 49L, 
                                trim = c(0.01, 0.99), 
                                strategy = c("uniform", "quantile"), na.rm = TRUE,
                                n_max = 1000L, w = NULL, ...) {
  if (is.null(pred_fun)) {
    pred_fun <- mlr3_pred_fun(object, X = X)
  }
  partial_dep.default(
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

#' @describeIn partial_dep Method for DALEX "explainer".
#' @export
partial_dep.explainer <- function(object, v, X = object[["data"]], 
                                  pred_fun = object[["predict_function"]],
                                  BY = NULL, by_size = 4L, grid = NULL, grid_size = 49L, 
                                  trim = c(0.01, 0.99), 
                                  strategy = c("uniform", "quantile"), na.rm = TRUE,
                                  n_max = 1000L, w = object[["weights"]], ...) {
  partial_dep.default(
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

#' Prints "partial_dep" Object
#' 
#' Print method for object of class "partial_dep".
#'
#' @param x An object of class "partial_dep".
#' @param n Number of rows to print.
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [partial_dep()] for examples.
print.partial_dep <- function(x, n = 3L, ...) {
  cat("Partial dependence object (", nrow(x[["data"]]), " rows). Extract via $data. Top rows:\n\n", sep = "")
  print(utils::head(x[["data"]], n))
  invisible(x)
}

#' Plots "partial_dep" Object
#' 
#' Plot method for objects of class "partial_dep". Can do (grouped) line plots or 
#' heatmaps.
#' 
#' @importFrom ggplot2 .data
#' @param x An object of class "partial_dep".
#' @param color Color of lines and points (in case there is no color/fill aesthetic).
#'   The default equals the global option `hstats.color = "#3b528b"`. 
#'   To change the global option, use `options(stats.color = new value)`.
#' @param swap_dim Switches the role of grouping and facetting (default is `FALSE`).
#'   Exception: For the 2D PDP with `d2_geom = "line"`, it swaps the role of the two
#'   variables in `v`.
#' @param show_points Logical flag indicating whether to show points (default) or not.
#'   No effect for 2D PDPs.
#' @param d2_geom The geometry used for 2D PDPs, by default "tile". Option "point"
#'   is useful, e.g., when the grid represents spatial points. Option "line" produces
#'   lines grouped by the second variable.
#' @param ... Arguments passed to geometries.
#' @inheritParams plot.hstats_matrix
#' @export
#' @returns An object of class "ggplot".
#' @seealso See [partial_dep()] for examples.
plot.partial_dep <- function(x,
                             color = getOption("hstats.color"),
                             swap_dim = FALSE,
                             viridis_args = getOption("hstats.viridis_args"),
                             facet_scales = "fixed",
                             rotate_x = FALSE, show_points = TRUE, 
                             d2_geom = c("tile", "point", "line"), ...) {
  d2_geom <- match.arg(d2_geom)
  v <- x[["v"]]
  by_name <- x[["by_name"]]
  K <- x[["K"]]
  if (length(v) > 2L) {
    stop("Maximal two features can be plotted.")
  }
  if (((K > 1L) + (!is.null(by_name)) + length(v)) > 3L) {
    stop("No plot implemented for this case.")
  }
  if (is.null(viridis_args)) {
    viridis_args <- list()
  }
  
  data <- with(x, poor_man_stack(data, to_stack = pred_names))

  if (length(v) == 2L && (K > 1L || !is.null(by_name))) {  # Only one is possible
    wrp <- if (K > 1L) "varying_" else by_name
  } else {
    wrp <- NULL
  }
  if (length(v) == 1L || d2_geom == "line") {
    # Line plots
    
    # Determine the role of x axis, color axis and facetting
    if (length(v) == 1L) {
      grp <- if (is.null(by_name) && K > 1L) "varying_" else by_name  # can be NULL
      wrp <- if (!is.null(by_name) && K > 1L) "varying_"
      if (swap_dim) {
        tmp <- grp
        grp <- wrp
        wrp <- tmp
      }
    } else {  # length(v) == 2
      if (swap_dim) {
        v <- rev(v)
      }
      grp <- v[2L]
      v <- v[1L]
    }

    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[v]], y = value_)) +
      ggplot2::labs(x = v, y = "PD")

    if (is.null(grp)) {
      p <- p + ggplot2::geom_line(color = color, group = 1, ...)
      if (show_points) {
        p <- p + ggplot2::geom_point(color = color)
      }
    } else {
      p <- p + 
        ggplot2::geom_line(
          ggplot2::aes(color = .data[[grp]], group = .data[[grp]]), ...
        ) +
        ggplot2::labs(color = grp) +
        do.call(get_color_scale(data[[grp]]), viridis_args)
      if (show_points) {
        p <- p + ggplot2::geom_point(
          ggplot2::aes(color = .data[[grp]], group = .data[[grp]])
        )
      }
      if (grp == "varying_") {
        p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
      }
    }
  } else if (length(v) == 2L) {
    # Heat maps ("tile" or "point", "line" has been treated above)
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[v[1L]]], y = .data[[v[2L]]]))
    if (d2_geom == "tile") {
      p <- p + ggplot2::geom_tile(ggplot2::aes(fill = value_), ...) +
        do.call(ggplot2::scale_fill_viridis_c, viridis_args) + 
        ggplot2::labs(fill = "PD")
    } else if (d2_geom == "point") {
      p <- p + ggplot2::geom_point(ggplot2::aes(color = value_), ...) +
        do.call(ggplot2::scale_color_viridis_c, viridis_args) + 
        ggplot2::labs(color = "PD")
    }
  }
  if (!is.null(wrp)) {
    p <- p + ggplot2::facet_wrap(wrp, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + rotate_x_labs()
  }
  p
}
