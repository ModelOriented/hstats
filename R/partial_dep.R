#' Partial Dependence Plot
#' 
#' Estimates the partial dependence function of feature(s) `v` over a 
#' grid of values. Both multivariate and multivariable situations are supported.
#' By default, the resulting values are plotted.
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
#' @inheritParams interact
#' @inheritParams multivariate_grid
#' @param grid A vector (if `length(v) == 1L`), or a matrix/data.frame otherwise.
#'   If `NULL`, calculated via [multivariate_grid()].
#' @param BY Optional grouping vector or a column name. The partial dependence
#'   function is calculated per `BY` group. Each `BY` group
#'   uses the same evaluation grid to improve assessment of (non-)additivity.
#'   Numeric `BY` variables with more than `by_size` disjoint values will be 
#'   binned into `by_size` quantile groups of similar size. Subsampling of `X` is done
#'   within group.
#' @param by_size Numeric `BY` variables with more than `by_size` unique values will
#'   be binned into quantile groups. Only relevant if `BY` is not `NULL`.
#' @returns 
#'   An object of class "partial_dep" containing these elements:
#'   - `pd`: data.frame containing the partial dependencies.
#'   - `v`: Same as input `v`.
#'   - `K`: Number of columns of prediction matrix.
#'   - `pred_names`: Column names of prediction matrix.
#'   - `BY`: Column name of grouping variable (or `NULL`).
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
#' # Stratified by numeric BY variable (which is automatically binned)
#' pd <- partial_dep(fit, v = "Species", X = iris, BY = "Petal.Length")
#' plot(pd)
#' 
#' # Multivariable input
#' v <- c("Species", "Petal.Width")
#' pd <- partial_dep(fit, v = v, X = iris, grid_size = 100L)
#' plot(pd, rotate_x = TRUE)
#' 
#' # With grouping
#' pd <- partial_dep(fit, v = v, X = iris, grid_size = 100L, BY = "Petal.Length")
#' plot(pd, rotate_x = TRUE)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' plot(partial_dep(fit, v = "Petal.Width", X = iris, BY = iris$Species))
#' plot(partial_dep(fit, v = c("Species", "Petal.Width"), X = iris), rotate_x = TRUE)
#' 
#' # Multivariate, multivariable, and BY (no plot available)
#' pd <- partial_dep(fit, v = c("Petal.Width", "Petal.Length"), X = iris, BY = "Species")
#' pd
#' 
#' # MODEL 3: Gamma GLM -> pass options to predict() via ...
#' fit <- glm(
#'   Sepal.Length ~ . + Petal.Width:Species, 
#'   data = iris, 
#'   family = Gamma(link = log)
#' )
#' plot(partial_dep(fit, v = "Species", X = iris, type = "response"))
partial_dep <- function(object, ...) {
  UseMethod("partial_dep")
}

#' @describeIn partial_dep Default method.
#' @export
partial_dep.default <- function(object, v, X, pred_fun = stats::predict, 
                                BY = NULL, by_size = 5L, grid = NULL, grid_size = 49L, 
                                trim = c(0.01, 0.99), 
                                strategy = c("uniform", "quantile"), n_max = 1000L, 
                                w = NULL, ...) {
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

    by_values <- unique(BY)
    if (is.numeric(BY) && length(by_values) > by_size) {
      BY <- qcut(BY, m = by_size)
      by_values <- unique(BY)
    }
    
    pd_list <- stats::setNames(vector("list", length = length(by_values)), by_values)
    for (b in by_values) {
      out <- partial_dep.default(
        object = object, 
        v = v, 
        X = X[BY %in% b, , drop = FALSE], 
        pred_fun = pred_fun,
        grid = grid,
        n_max = n_max, 
        w = if (!is.null(w)) w[BY %in% b],
        ...
      )
      pd_list[[b]] <- out[["pd"]]
    }
    pd <- do.call(rbind, c(pd_list, list(make.row.names = FALSE)))
    BY_rep <- rep(by_values, times = vapply(pd_list, nrow, FUN.VALUE = 1L))
    BY_rep <- stats::setNames(as.data.frame(BY_rep), by_name)
    out[["pd"]] <- cbind.data.frame(BY_rep, pd)
    out[["BY"]] <- by_name
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
    object = object, 
    v = v, 
    X = X, 
    grid = grid,
    pred_fun = pred_fun,
    w = w,
    compress_grid = FALSE,  # Almost always unique, so we save a check for uniqueness
    ...
  )
  K <- ncol(pd)
  if (is.null(colnames(pd))) {
    colnames(pd) <- if (K == 1L) "y" else paste0("y", seq_len(K))
  }
  if (!is.data.frame(grid) && !is.matrix(grid)) {
    grid <- stats::setNames(as.data.frame(grid), v)
  }
  
  out <- list(
    pd = cbind.data.frame(grid, pd),
    v = v,
    K = K,
    pred_names = colnames(pd),
    BY = NULL
  )
  return(structure(out, class = "partial_dep"))
}

#' @describeIn partial_dep Method for "ranger" models.
#' @export
partial_dep.ranger <- function(object, v, X, 
                               pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                               BY = NULL, by_size = 5L, grid = NULL, grid_size = 49L, 
                               trim = c(0.01, 0.99), 
                               strategy = c("uniform", "quantile"), n_max = 1000L, 
                               w = NULL, ...) {
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
    n_max = n_max,
    w = w,
    ...
  )
}

#' @describeIn partial_dep Method for "mlr3" models.
#' @export
partial_dep.Learner <- function(object, v, X, 
                                pred_fun = function(m, X) m$predict_newdata(X)$response,
                                BY = NULL, by_size = 5L, grid = NULL, grid_size = 49L, 
                                trim = c(0.01, 0.99), 
                                strategy = c("uniform", "quantile"), n_max = 1000L, 
                                w = NULL, ...) {
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
#' @param n Number of rows of partial dependencies to show.
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [partial_dep()] for examples.
print.partial_dep <- function(x, n = 3L, ...) {
  cat("Partial dependence object (", nrow(x[["pd"]]), " rows). 
      Extract via $pd. Top rows:\n\n", sep = "")
  print(utils::head(x[["pd"]], n))
  invisible(x)
}

#' Plots "partial_dep" Object
#' 
#' Plot method for objects of class "partial_dep". Can do (grouped) line plots or 
#' heatmaps.
#' 
#' @importFrom ggplot2 .data
#' @param x An object of class "partial_dep".
#' @param rotate_x Should x axis labels be rotated by 45 degrees?
#' @param color Color of lines and points (in case there is no color/fill aesthetic).
#' @param facet_scales Value passed to `facet_wrap(scales = ...)`.
#' @param ... Arguments passed to geometries.
#' @export
#' @returns An object of class "ggplot".
#' @seealso See [partial_dep()] for examples.
plot.partial_dep <- function(x, rotate_x = FALSE, color = "#2b51a1", 
                             facet_scales = "free_y", ...) {
  v <- x[["v"]]
  BY <- x[["BY"]]
  K <- x[["K"]]
  if (length(v) > 2L) {
    stop("Maximal two features can be plotted.")
  }
  if ((K > 1L) + (!is.null(BY)) + length(v) > 3L) {
    stop("No plot implemented in this case.")
  }
  data <- with(x, poor_man_stack(pd, to_stack = pred_names))
  
  # Line plots
  if (length(v) == 1L) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[v]], y = value_)) +
      ggplot2::labs(x = v, y = "PD")
    
    if (is.null(BY)) {
      p <- p + 
        ggplot2::geom_line(color = color, group = 1, ...) +
        ggplot2::geom_point(color = color, ...)
    } else {
      p <- p + 
        ggplot2::geom_line(
          ggplot2::aes(color = .data[[BY]], group = .data[[BY]]), ...
        ) +
        ggplot2::geom_point(
          ggplot2::aes(color = .data[[BY]], group = .data[[BY]]), ...
        ) +
        ggplot2::labs(color = BY)
    }
    if (K > 1L) {
      p <- p + ggplot2::facet_wrap(~ varying_, scales = facet_scales)
    }
  } else if (length(v) == 2L) {
    # Heat maps
    p <- ggplot2::ggplot(
      data, ggplot2::aes(x = .data[[v[1L]]], y = .data[[v[2L]]], fill = value_)
    ) + 
      ggplot2::geom_tile(...) +
      ggplot2::labs(fill = "PD")
    
    if (K > 1L) {
      p <- p + ggplot2::facet_wrap(~ varying_, scales = facet_scales)
    } else if (!is.null(BY)) {
      p <- p + ggplot2::facet_wrap(BY, scales = facet_scales)
    }
  }
  if (rotate_x) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )  
  }
  p
}
