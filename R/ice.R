#' Individual Conditional Expectations
#'
#' Disaggregated partial dependencies, see reference. The plot method supports
#' up to two grouping variables via `BY`.
#'
#' @inheritParams partial_dep
#' @param v One or more column names over which you want to calculate the ICE.
#' @param BY Optional grouping vector/matrix/data.frame (up to two columns), 
#'   or up to two column names. Unlike with [partial_dep()], these variables are not
#'   binned. The first variable is visualized on the color scale, while the second
#'   one goes into a `facet_wrap()`. Thus, make sure that the second variable is
#'   discrete.
#' @returns
#'   An object of class "ice" containing these elements:
#'   - `data`: data.frame containing the ice values.
#'   - `grid`: Vector, matrix or data.frame of grid values.
#'   - `v`: Same as input `v`.
#'   - `K`: Number of columns of prediction matrix.
#'   - `pred_names`: Column names of prediction matrix.
#'   - `by_names`: Column name(s) of grouping variable(s) (or `NULL`).
#' @references
#'   Goldstein, Alex, and Adam Kapelner and Justin Bleich and Emil Pitkin.
#'     *Peeking inside the black box: Visualizing statistical learning with plots of individual conditional expectation.*
#'     Journal of Computational and Graphical Statistics, 24, no. 1 (2015): 44-65.
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ . + Species * Petal.Length, data = iris)
#' plot(ice(fit, v = "Sepal.Width", X = iris))
#'
#' # Stratified by one variable
#' ic <- ice(fit, v = "Petal.Length", X = iris, BY = "Species")
#' ic
#' plot(ic)
#' plot(ic, center = TRUE)
#' 
#' # Stratified by two variables (the second one goes into facets)
#' ic <- ice(fit, v = "Petal.Length", X = iris, BY = c("Petal.Width", "Species"))
#' plot(ic)
#' plot(ic, center = TRUE)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' ic <- ice(fit, v = "Petal.Width", X = iris, BY = iris$Species)
#' plot(ic)
#' plot(ic, center = TRUE)
#' plot(ic, swap_dim = TRUE)
#'
#' # MODEL 3: Gamma GLM -> pass options to predict() via ...
#' fit <- glm(Sepal.Length ~ ., data = iris, family = Gamma(link = log))
#' plot(ice(fit, v = "Petal.Length", X = iris, BY = "Species"))
#' plot(ice(fit, v = "Petal.Length", X = iris, type = "response", BY = "Species"))
ice <- function(object, ...) {
  UseMethod("ice")
}

#' @describeIn ice Default method.
#' @export
ice.default <- function(object, v, X, pred_fun = stats::predict,
                        BY = NULL, grid = NULL, grid_size = 49L,
                        trim = c(0.01, 0.99),
                        strategy = c("uniform", "quantile"), n_max = 100L, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    is.function(pred_fun),
    all(v %in% colnames(X))
  )
  
  # Prepare grid
  if (is.null(grid)) {
    grid <- multivariate_grid(
      x = X[, v], grid_size = grid_size, trim = trim, strategy = strategy
    )
  } else {
    check_grid(g = grid, v = v, X_is_matrix = is.matrix(X))
  }
  
  # Prepare BY (could be integrated into prepare_by())
  if (!is.null(BY)) {
    if (length(BY) <= 2L && all(BY %in% colnames(X))) {
      by_names <- BY
      BY <- X[, BY]
    } else {
      stopifnot(
        NROW(BY) == nrow(X),
        NCOL(BY) <= 2L
      )
      by_names <- colnames(BY)
      if (is.null(by_names)) {
        n_by <- NCOL(BY)
        by_names = if (n_by == 1L) "Group" else paste0("Group_", seq_len(n_by))
      }
    }
    if (!is.data.frame(BY)) {
      BY <- as.data.frame(BY)
    }
  } else {
    by_names <- NULL
  }
  
  # Reduce size of X (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(BY)) {
      BY <- BY[ix, , drop = FALSE]
    }
  }

  ice_out <- ice_raw(
    object, v = v, X = X, grid = grid, pred_fun = pred_fun, pred_only = FALSE, ...
  )
  pred <- ice_out[["pred"]]
  grid_pred <- ice_out[["grid_pred"]]
  K <- ncol(pred)
  if (is.null(colnames(pred))) {
    colnames(pred) <- if (K == 1L) "y" else paste0("y", seq_len(K))
  }
  pred_names <- colnames(pred)
  if (!is.data.frame(grid_pred) && !is.matrix(grid_pred)) {
    grid_pred <- stats::setNames(as.data.frame(grid_pred), v)
  }
  ice_curves <- cbind.data.frame(obs_ = seq_len(nrow(X)), grid_pred, pred)
  if (!is.null(BY)) {
    ice_curves[by_names] <- BY[rep(seq_len(nrow(BY)), times = NROW(grid)), ]
  }
  row.names(ice_curves) <- NULL  # could be solved before
  out <- list(
    data = ice_curves,
    grid = grid,
    v = v, 
    K = K,
    pred_names = pred_names, 
    by_names = by_names
  )
  return(structure(out, class = "ice"))
}

#' @describeIn ice Method for "ranger" models.
#' @export
ice.ranger <- function(object, v, X,
                       pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                       BY = NULL, grid = NULL, grid_size = 49L,
                       trim = c(0.01, 0.99),
                       strategy = c("uniform", "quantile"), n_max = 100, ...) {
  ice.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    BY = BY,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    n_max = n_max,
    ...
  )
}

#' @describeIn ice Method for "mlr3" models.
#' @export
ice.Learner <- function(object, v, X,
                        pred_fun = NULL,
                        BY = NULL, grid = NULL, grid_size = 49L, trim = c(0.01, 0.99),
                        strategy = c("uniform", "quantile"), n_max = 100L, ...) {
  if (is.null(pred_fun)) {
    pred_fun <- mlr3_pred_fun(object, X = X)
  }
  ice.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    BY = BY,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    n_max = n_max,
    ...
  )
}

#' @describeIn ice Method for DALEX "explainer".
#' @export
ice.explainer <- function(object, v = v, X = object[["data"]],
                          pred_fun = object[["predict_function"]],
                          BY = NULL, grid = NULL, grid_size = 49L,
                          trim = c(0.01, 0.99),
                          strategy = c("uniform", "quantile"), n_max = 100, ...) {
  ice.default(
    object = object[["model"]],
    v = v,
    X = X,
    pred_fun = pred_fun,
    BY = BY,
    grid = grid,
    grid_size = grid_size,
    trim = trim,
    strategy = strategy,
    n_max = n_max,
    ...
  )
}

#' Prints "ice" Object
#'
#' Print method for object of class "ice".
#'
#' @param x An object of class "ice".
#' @param n Number of rows to print.
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [ice()] for examples.
print.ice <- function(x, n = 3L, ...) {
  cat("'ice' object (", nrow(x[["data"]]), " rows). Extract via $data. Top rows:\n\n", sep = "")
  print(utils::head(x[["data"]], n))
  invisible(x)
}

#' Plots "ice" Object
#'
#' Plot method for objects of class "ice".
#'
#' @importFrom ggplot2 .data
#' @inheritParams plot.hstats_matrix
#' @inheritParams plot.partial_dep
#' @param x An object of class "ice".
#' @param center Should curves be centered? Default is `FALSE`.
#' @param alpha Transparency passed to `ggplot2::geom_line()`.
#' @param swap_dim Swaps between color groups and facets. Default is `FALSE`.
#' @export
#' @returns An object of class "ggplot".
#' @seealso See [ice()] for examples.
plot.ice <- function(x, center = FALSE, alpha = 0.2, 
                     color = getOption("hstats.color"),
                     swap_dim = FALSE,
                     viridis_args = getOption("hstats.viridis_args"),
                     facet_scales = "fixed", 
                     rotate_x = FALSE, ...) {
  v <- x[["v"]]
  K <- x[["K"]]
  data <- x[["data"]]
  pred_names <- x[["pred_names"]]
  by_names <- x[["by_names"]]
  if (is.null(viridis_args)) {
    viridis_args <- list()
  }
  
  if (length(v) > 1L) {
    stop("Maximal one feature v can be plotted.")
  }
  if ((K > 1L) + length(by_names) > 2L) {
    stop("Two BY variables of multivariate output is not supported yet.")
  }
  if (center) {
    pos <- trunc((NROW(x[["grid"]]) + 1) / 2)
    data[pred_names] <- lapply(
      data[pred_names], 
      function(z) stats::ave(z, data[["obs_"]], FUN = function(zz) zz - zz[pos])
    )
  }
  data <- poor_man_stack(data, to_stack = pred_names)

  # Distinguish all possible cases
  grp <- if (is.null(by_names) && K > 1L) "varying_" else by_names[1L]   # can be NULL
  wrp <- if (!is.null(by_names) && K > 1L) "varying_" 
  if (length(by_names) == 2L) {
    wrp <- by_names[2L]
  }
  if (swap_dim) {
    tmp <- grp
    grp <- wrp
    wrp <- tmp
  }
  
  if (!is.null(grp) && grp == "varying_") {
    data <- transform(data, obs_ = interaction(obs_, varying_))
  }
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[v]], y = value_, group = obs_)) +
    ggplot2::labs(x = v, y = if (center) "Centered ICE" else "ICE")
  
  if (is.null(grp)) {
    p <- p + ggplot2::geom_line(color = color, alpha = alpha, ...)
  } else {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(color = .data[[grp]]), alpha = alpha, ...) +
      ggplot2::labs(color = grp) + 
      do.call(get_color_scale(data[[grp]]), viridis_args) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(alpha = 1)))
    if (grp == "varying_") {
      p <- p + ggplot2::theme(legend.title = ggplot2::element_blank())
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
