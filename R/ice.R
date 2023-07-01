#' Individual Conditional Expectations
#'
#' Estimates the partial dependence function of feature(s) `v` over a
#' grid of values. Both multivariate and multivariable situations are supported.
#' By default, the resulting values are plotted.
#'
#' @inheritParams partial_dep
#' @param BY Optional grouping vector or a column name.
#' @param w Currently unused.
#' @param center Should ICE curves be centered at some middle grid value? 
#'   Default is `TRUE`.
#' @returns
#'   An object of class "ice" containing these elements:
#'   - `ice_curves`: data.frame containing the ice values.
#'   - `v`: Same as input `v`.
#'   - `K`: Number of columns of prediction matrix.
#'   - `pred_names`: Column names of prediction matrix.
#'   - `BY`: Column name of grouping variable (or `NULL`).
#'   - `center`: Same as input `center`.
#' @references
#'   Goldstein, A. et al. (2015). Peeking inside the black box: Visualizing statistical
#'     learning with plots of individual conditional expectation.
#'     Journal of Computational and Graphical Statistics, 24:1
#'     <doi.org/10.1080/10618600.2014.907095>.
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ . + Species * Petal.Length, data = iris)
#' (ic <- ice(fit, v = "Species", X = iris))
#' plot(ic)
#'
#' # Stratified by numeric BY variable
#' ic <- ice(fit, v = "Species", X = iris, BY = "Petal.Length")
#' plot(ic)
#'
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' plot(ice(fit, v = "Petal.Width", X = iris, BY = iris$Species))
#'
#' # MODEL 3: Gamma GLM -> pass options to predict() via ...
#' fit <- glm(
#'   Sepal.Length ~ . + Petal.Width:Species,
#'   data = iris,
#'   family = Gamma(link = log)
#' )
#' plot(ice(fit, v = "Petal.Width", X = iris, type = "response"))
ice <- function(object, ...) {
  UseMethod("ice")
}

#' @describeIn ice Default method.
#' @export
ice.default <- function(object, v, X, pred_fun = stats::predict,
                        BY = NULL, grid = NULL, grid_size = 49L,
                        trim = c(0.01, 0.99),
                        strategy = c("uniform", "quantile"), n_max = 100L,
                        w = NULL, center = TRUE, ...) {
  basic_check(X = X, v = v, pred_fun = pred_fun, w = w)

  if (is.null(grid)) {
    grid <- multivariate_grid(
      x = X[, v], grid_size = grid_size, trim = trim, strategy = strategy
    )
  } else {
    check_grid(g = grid, v = v, X_is_matrix = is.matrix(X))
  }

  if (!is.null(BY)) {
    if (length(BY) == 1L && BY %in% colnames(X)) {
      by_name <- BY
      BY <- X[, by_name]
    } else {
      by_name = "Group"
      if (length(BY) != nrow(X)) {
        stop("BY variable must have same length as X.")
      }
    }
  } else {
    by_name <- NULL
  }

  # Reduce size of X (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(BY)) {
      BY <- BY[ix]
    }
    if (!is.null(w)) {
      w <- w[ix]
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
  if (!is.data.frame(grid_pred) && !is.matrix(grid_pred)) {
    grid_pred <- stats::setNames(as.data.frame(grid_pred), v)
  }
  ice_curves <- cbind.data.frame(obs_ = seq_len(nrow(X)), grid_pred, pred)
  if (!is.null(BY)) {
    BY <- stats::setNames(as.data.frame(BY), by_name)
    ice_curves <- cbind.data.frame(BY, ice_curves)
  }
  out <- list(
    ice_curves = ice_curves, 
    v = v, 
    K = K, 
    pred_names = colnames(pred), 
    BY = by_name,
    center = center
  )
  return(structure(out, class = "ice"))
}

#' @describeIn ice Method for "ranger" models.
#' @export
ice.ranger <- function(object, v, X,
                       pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                       BY = NULL, grid = NULL, grid_size = 49L,
                       trim = c(0.01, 0.99),
                       strategy = c("uniform", "quantile"), n_max = 100, w = NULL,
                       center = TRUE, ...) {
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
    w = w,
    center = center,
    ...
  )
}

#' @describeIn ice Method for "mlr3" models.
#' @export
ice.Learner <- function(object, v, X,
                        pred_fun = function(m, X) m$predict_newdata(X)$response,
                        BY = NULL, grid = NULL, grid_size = 49L, trim = c(0.01, 0.99),
                        strategy = c("uniform", "quantile"), n_max = 100L,
                        w = NULL, center = TRUE, ...) {
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
    w = w,
    center = center,
    ...
  )
}

#' Prints "ice" Object
#'
#' Print method for object of class "ice".
#'
#' @param x An object of class "ice".
#' @param n Number of rows of partial dependencies to show.
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [ice()] for examples.
print.ice <- function(x, n = 3L, ...) {
  cat(if (x[["center"]]) "Centered ", "'ice' object (", nrow(x[["ice_curves"]]), " rows).
      Extract via $ice_curves. Top rows:\n\n", sep = "")
  print(utils::head(x[["ice_curves"]], n))
  invisible(x)
}

#' Plots "ice" Object
#'
#' Plot method for objects of class "ice". Can do (grouped) line plots or
#' heatmaps.
#'
#' @importFrom ggplot2 .data
#' @inheritParams plot.partial_dep
#' @param x An object of class "ice".
#' @param alpha Transparency passed to geometries.
#' @export
#' @returns An object of class "ggplot".
#' @seealso See [ice()] for examples.
plot.ice <- function(x, alpha = 0.2, rotate_x = FALSE, color = "#2b51a1",
                     facet_scales = "free_y", ...) {
  v <- x[["v"]]
  BY <- x[["BY"]]
  K <- x[["K"]]
  if (length(v) > 1L) {
    stop("Maximal one feature can be plotted.")
  }
  data <- with(x, poor_man_stack(ice_curves, to_stack = pred_names))

  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[v]], y = value_, group = obs_)) +
    ggplot2::labs(x = v, y = "ICE")

  if (is.null(BY)) {
    p <- p +
      ggplot2::geom_line(color = color, alpha = alpha, ...)
  } else {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(color = .data[[BY]]), alpha = alpha, ...
      ) +
      ggplot2::labs(color = BY)
  }
  if (K > 1L) {
    p <- p + ggplot2::facet_wrap(~ varying_, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )
  }
  p
}
