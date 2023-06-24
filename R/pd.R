#' Partial Dependence Function
#' 
#' Estimates the partial dependence function of feature(s) `v` over a 
#' grid of values. Both multivariate and multivariable situations are supported.
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
#' @param object Fitted model object.
#' @param v Vector of feature names.
#' @param X A data.frame or matrix serving as background dataset.
#' @param grid A vector (if `length(v) == 1L`), or a matrix/data.frame otherwise.
#'   If `NULL`, calculated via [multivariate_grid()].
#' @param pred_fun Prediction function of the form `function(object, X, ...)`,
#'   providing K >= 1 numeric predictions per row. Its first argument represents the 
#'   model `object`, its second argument a data structure like `X`. Additional arguments 
#'   (such as `type = "response"` in a GLM) can be passed via `...`. The default, 
#'   [stats::predict()], will work in most cases. Note that column names in a resulting
#'   matrix of predictions will be used as default column names in the results.
#' @param BY Optional grouping vector or a column name. The partial dependence
#'   function is calculated per `BY` group. Each `BY` group
#'   uses the same evaluation grid to improve assessment of (non-)additivity.
#'   Numeric `BY` variables with more than `by_size` disjoint values will be 
#'   binned into `by_size` quantile groups of similar size.
#' @param by_size Numeric `BY` variables with more than `by_size` unique values will
#'   be binned into quantile groups. Only relevant if `BY` is not `NULL`.
#' @param n_max If `X` has more than `n_max` rows, a random sample of `n_max` rows is
#'   selected from `X`. In this case, set a random seed for reproducibility.
#' @param w Optional vector of case weights for each row of `X`.
#' @param grid Optional evaluation grid in line with `v`.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`, 
#'   for instance `type = "response"` in a [glm()] model.
#' @returns 
#'   A list with additional class "pd" containing the following elements
#'   - `pd`: data.frame representing the evaluation grid and the corresponding
#'     partial dependencies. (And the optional `BY` variable.)
#'   - `v`: Same as input `v`.
#'   - `pred_names`: Vector of column names representing partial dependencies.
#'   - `by_name`: Name of the `BY` column in the resulting data.frame.
#' @references
#'   Friedman, Jerome H. *"Greedy Function Approximation: A Gradient Boosting Machine."* 
#'     Annals of Statistics 29, no. 5 (2001): 1189-1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Species * Petal.Length, data = iris)
#' pd <- partial_dep(fit, v = "Species", X = iris)
#' pd
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(pd)
#' }
#' 
#' # Stratified by numeric BY variable (which is automatically binned)
#' pd <- partial_dep(fit, v = "Species", X = iris, BY = iris$Petal.Length)
#' pd
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(pd)
#' }
#' 
#' # Multivariable input (no plots available)
#' pd <- partial_dep(fit, v = c("Species", "Petal.Width"), X = iris)
#' pd
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' pd <- partial_dep(fit, v = "Petal.Width", X = iris, BY = "Species")
#' pd
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(pd)
#' }
#' 
#' # Multivariate, multivariable, and BY
#' pd <- partial_dep(
#'   fit, v = c("Petal.Width", "Petal.Length"), X = iris, BY = "Species"
#' )
#' pd
#'  
#' # MODEL THREE: Gamma GLM -> pass options to predict() via ...
#' fit <- glm(
#'   Sepal.Length ~ . + Petal.Width:Species, 
#'   data = iris, 
#'   family = Gamma(link = log)
#' )
#' partial_dep(fit, v = "Species", X = iris, type = "response")$pd
partial_dep <- function(object, ...) {
  UseMethod("partial_dep")
}

#' @describeIn partial_dep Default method.
#' @export
partial_dep.default <- function(object, v, X, pred_fun = stats::predict, 
                                BY = NULL, by_size = 5L,
                                grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                                strategy = c("uniform", "quantile"), 
                                n_max = 1000L, w = NULL, ...) {
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
      BY <- X[, BY]
    } else {
      by_name = "Group"
    }
    if (length(BY) != nrow(X)) {
      stop("BY variable must have same length as X.")
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
    out[["by_name"]] <- by_name
    return(out)
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
  if (is.null(colnames(pd))) {
    K <- ncol(pd)
    colnames(pd) <- if (K == 1L) "y" else paste0("y", seq_len(K))
  }
  if (!is.data.frame(grid) && !is.matrix(grid)) {
    grid <- stats::setNames(as.data.frame(grid), v)
  }
  structure(
    list(
      pd = cbind.data.frame(grid, pd), 
      v = v, 
      pred_names = colnames(pd), 
      by_name = NULL
      ), 
    class = "pd"
  ) 
}

#' @describeIn partial_dep Method for "ranger" models.
#' @export
partial_dep.ranger <- function(object, v, X, 
                               pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                               BY = NULL, by_size = 5L,
                               grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
                               strategy = c("uniform", "quantile"), 
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
    n_max = n_max,
    w = w,
    ...
  )
}

#' @describeIn partial_dep Method for "mlr3" models.
#' @export
partial_dep.Learner <- function(object, v, X, 
                                pred_fun = function(m, X) m$predict_newdata(X)$response,
                                BY = NULL, by_size = 5L,
                                grid = NULL, grid_size = 36L, trim = c(0.01, 0.99),
                                strategy = c("uniform", "quantile"), 
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
    n_max = n_max,
    w = w,
    ...
  )
}

#' Print Method
#' 
#' Print method for object of class "pd". 
#'
#' @param x An object of class "pd".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [partial_dep()] for examples.
print.pd <- function(x, ...) {
  cat("First partial dependence rows (use x$pd to extract all)\n\n")
  print(utils::head(x[["pd"]]))
  invisible(x)
}

#' PDP
#' 
#' Plot method for object of class "pd". It supports multivariate predictions and a 
#' BY variable, but only univariable `v`.
#'
#' @param x An object of class "pd".
#' @param rotate_x Should x axis labels be rotated by 45 degrees?
#' @param color Color of lines and points.
#' @param facet_scales Value passed to `facet_wrap(scales = ...)`, only relevant
#'   for multivariate output.
#' @param ... Further arguments passed to geometries.
#' @returns An object of class "ggplot".
#' @export
#' @seealso See [partial_dep()] for examples.
plot.pd <- function(x, rotate_x = FALSE, color = "#2b51a1", 
                    facet_scales = "free_y", ...) {
  v <- x[["v"]]
  data <- x[["pd"]]
  pred_names <- x[["pred_names"]]
  by_name <- x[["by_name"]]

  if (length(v) > 1L) {
    stop("No plot method for multivariable case (more than one 'v')")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install 'ggplot2' to use plot().")
  }
  data <- poor_man_stack(data, to_stack = pred_names)
  
  # Renaming to avoid importing .data from {rlang}
  data[["v_"]] <- data[[v]]
  if (!is.null(by_name)) {
    data[["by_"]] <- data[[by_name]]
  }

  p <- ggplot2::ggplot(data, ggplot2::aes(x = v_, y = value_))
  if (is.null(by_name)) {
    p <- p + 
      ggplot2::geom_line(color = color, group = 1, ...) +
      ggplot2::geom_point(color = color, ...)
  } else {
    p <- p + 
      ggplot2::geom_line(ggplot2::aes(color = by_, group = by_), ...) +
      ggplot2::geom_point(ggplot2::aes(color = by_, group = by_), ...)
  }
  
  if (length(pred_names) > 1L) {
    p <- p + ggplot2::facet_wrap(~ varying_, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )  
  }
  p + ggplot2::labs(x = v, y = "PD", color = by_name)
}
