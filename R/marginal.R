#' Marginal Statistics
#' 
#' Calculates 
#' - average observed, 
#' - average predicted,
#' - partial dependence, and
#' - counts/weights
#' over a (binned) feature v, possibly weigthed by a vector `w`.
#' 
#'   
#' 
#' @inheritParams hstats
#' @param v One or more column names over which you want to calculate the partial
#'   dependence.
#' @returns 
#'   An object of class "marginal" containing these elements:
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
#' library(plotly)
#' library(ranger)
#' 
#' fit <- ranger(Sepal.Length ~ ., data = iris)
#' pf <- function(m, x) predict(m, x)$predictions
#' 
#' x <- marginal.default(
#'   fit,
#'   v = 'Sepal.Width',
#'   X = iris,
#'   y = "Sepal.Length",
#'   pred_fun = pf,
#'   breaks = 5,
#'   max_bins_discrete = 2
#' )
#' x
#' x |> plot(backend = "plotly")
#' x |> plot(exposure = 1)
marginal <- function(object, ...) {
  UseMethod("marginal")
}

#' @describeIn marginal Default method.
#' @export
marginal.default <- function(
    object,
    v,
    X,
    y = NULL,                 #  NULL to switch off
    pred = NULL,
    pred_fun = stats::predict,
    w = NULL,
    breaks = "FD",
    right = TRUE,
    max_bins_discrete = 28,
    pd_n = 1000L,             #  0 to switch off
    ...
) {
  stopifnot(
    is.data.frame(X) || is.matrix(X),
    is.function(pred_fun),
    nrow(X) >= 2L,
    v %in% colnames(X)
  )
  
  # Prepare pred (can't stay NULL)
  if (is.null(pred)) {
    pred <- prepare_pred2(pred_fun(object, X, ...))
  } else {
    pred <- prepare_pred2(pred)
    if (length(pred) != nrow(X)) {
      stop("'pred' should be a vector of length nrow(X), or NULL.")
    }
  }
  
  # Prepare y (can stay NULL)
  if (!is.null(y)) {
    y <- prepare_yw(y, X)
  }
  
  # Prepare w (can stay NULL)
  if (!is.null(w)) {
    w <- prepare_w(w, X = X)[["w"]]
    if (any(w < 0) || anyNA(w)) {
      stop("'w' can't have negative or missing values")
    }
  }
  
  # prepare v
  if (length(v) != 1L || !(v %in% colnames(X))) {
    stop("'v' must be a column name in 'X'.")
  }
  vv <- if (is.matrix(X)) X[, v] else X[[v]]
  
  grid <- unique(vv)
  
  if (is.numeric(vv) && length(grid) > max_bins_discrete) {
    H <- graphics::hist(
      vv,
      breaks = breaks,
      include.lowest = TRUE,
      right = right,
      plot = FALSE
    )
    grid <- H$mids      # Sorted, i.e. same order as gwColMeans()
    vv <- grid[
      findInterval(
        vv, H$breaks, rightmost.closed = TRUE, left.open = right, all.inside = TRUE
      )
    ]
    bar_width <- diff(H$breaks)
    is_discrete <- FALSE
  } else {
    grid <- sort(grid)    # Same order as gwColMeans(). TODO: Check factors
    if (anyNA(x)) {
      grid <- c(grid, NA)
    }
    bar_width <- 0.7
    is_discrete <- TRUE
  }
  
  # Get statistics and attach info
  out <- gwColMeans(cbind(pred = pred, y = y), g = vv, w = w)
  out <- cbind.data.frame(grid, out$w, bar_width, out$M)
  colnames(out) <- c("v", "exposure", "bar_width", "pred", if (!is.null(y)) "obs")
  rownames(out) <- NULL
  
  # Partial dependence
  if (pd_n >= 1L) {
    if (nrow(X) > pd_n) {
      ix <- sample(nrow(X), pd_n)
      X <- X[ix, , drop = FALSE]
      if (!is.null(w)) {
        w <- w[ix]
      }
    }
    out$pd <- wrowmean_vector(
      ice_raw(
        object = object,
        v = v,
        X = X,
        grid = grid,
        pred_fun = pred_fun,
        pred_only = TRUE,
        ...
      ),
      ngroups = length(grid),
      w = w
    )
  }
  
  if (is_discrete && is.numeric(grid)) {
    out[[v]] <- factor(out[[v]])
  }
  
  # Combine everything
  structure(
    list(data = out, v = v, is_discrete = is_discrete),
    class = "marginal"
  )
}

prepare_pred2 <- function(x) {
  if (NCOL(x) > 1L) {
    stop("Only univariate predictions are handled.")
  }
  if (is.data.frame(x)) {
    x <- x[[1L]]
  }
  if (!is.vector(x)) {
    x <- as.vector(x)
  }
  if (!is.numeric(x) && !is.logical(x)) {
    stop("Predictions must be numeric or logical.")
  }
  return(as.double(x))
}

prepare_yw <- function(z, X) {
  if (length(z) == 1L && z %in% colnames(X)) {
    z <- if (is.matrix(X)) X[, z] else X[[z]]
  } else if (length(z) != nrow(X)) {
    stop("'y' or 'w' should either be NULL, a variable name, or have length nrow(X)")
  }
  return(prepare_pred2(z))
}


#' @describeIn marginal Method for "ranger" models.
#' @export
marginal.ranger <- function() {
  print("Todo")
  # marginal.default()
}

#' @describeIn marginal Method for DALEX "explainer".
#' @export
marginal.explainer <- function() {
  print("Todo")
  # marginal.default()
}

#' Prints "marginal" Object
#' 
#' Print method for object of class "marginal".
#'
#' @param x An object of class "marginal".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [marginal()] for examples.
print.marginal <- function(x, ...) {
  cat("marginal object: \n", sep = "")
  print(x$data)
  invisible(x)
}


#' Plots "marginal" Object
#'
#' Plots all information calculated from [marginal()] using a color blind palette from
#' {ggthemes}. When {plotly} is installed, you can switch to the interactive interface 
#' by setting `backend = "plotly"`. In this case, the result is expected to be better
#' than via `ggplotly()`.
#' 
#' Bars can be switched off via `exposure = FALSE`. Single lines can be switched off
#' by passing a shorter `line_colors` vector.
#' 
#' @importFrom ggplot2 .data
#' @param x An object of class "marginal".
#' @param line_colors Named vector of line colors. By default, a color blind
#'   palette from {ggthemes} is used. Can be used to remove certain lines in the plot.
#' @param fill Fill color of bars. Default is "lightgrey".
#' @param rotate_x Should x axis labels be rotated by 45 degrees (only ggplot2)?
#' @param show_exposure Should exposure bars be shown (on hidden right y axis)?
#'   For `backend = "ggplot"`, a value between 0 and 1 scales the bar heights so they
#'   would cover only part of the y range.
#' @param drop_below Drop values with lower exposure. The default 0 keeps all.
#' @param na.rm Should values for missing v be plotted (on the very right)?
#'   Default is `TRUE`.
#' @param backend Plot backend, one of "ggplot2" or "plotly". TODO: Make global option.
#' @param ... Currently not used.
#' @export
#' @returns An object of class "ggplot" or "plotly"/"htmlwidget".
plot.marginal <- function(
    x, 
    line_colors = c(obs = "#E69F00", pred = "#009E73", pd = "#56B4E9"),
    fill = "lightgrey",
    rotate_x = FALSE,
    show_exposure = TRUE,
    drop_below = 0,
    na.rm = FALSE,
    backend = c("ggplot2", "plotly"),
    ...
  ) {
  backend <- match.arg(backend)
  vars_to_show <- Reduce(
    intersect, list(c("obs", "pred", "pd"), colnames(x$data), names(line_colors))
  )
  
  if (drop_below > 0) {
    x$data <- x$data[x$data$exposure >= drop_below, ]
  }
  if (isTRUE(na.rm)) {
    x$data <- x$data[!is.na(x$data$v), ]
  }
  
  if (backend == "plotly") {
    p <- plot_marginal_plotly(
      x,
      vars_to_show = vars_to_show,
      line_colors = line_colors,
      fill = fill,
      show_exposure = show_exposure
    )
    return(p)
  }
  plot_marginal_ggplot(
    x,
    vars_to_show = vars_to_show,
    line_colors = line_colors,
    fill = fill,
    rotate_x = rotate_x,
    show_exposure = show_exposure,
    ...
  )
}

plot_marginal_plotly <- function(x, vars_to_show, line_colors, fill, show_exposure, ...) {
  fig <- plot_ly(x = x$data$v)

  if (show_exposure) {
    fig <- add_bars(
      fig,
      y = x$data$exposure,
      yaxis = "y2",
      color = I(fill),
      name = "exposure",
      showlegend = FALSE,
      width = x$data$bar_width
    )
  }

  for (z in vars_to_show) {
    fig <- add_trace(
      fig,
      y = x$data[[z]],
      mode = "lines+markers",
      type = "scatter",
      name = z,
      color = I(line_colors[z])
    )
  }
  
  fig <- layout(
    fig,
    yaxis2 = list(side = "right", showgrid = FALSE, showticklabels = FALSE),
    yaxis = list(title = "Response", overlaying = "y2"),
    xaxis = list(title = x$v),
    legend = list(orientation = "v", x = 1.05, xanchor = "left")
  )
  fig
}

plot_marginal_ggplot <- function(
    x,
    vars_to_show,
    line_colors,
    fill,
    rotate_x,
    show_exposure,
    ...
  ) {
  df <- poor_man_stack(x$data, vars_to_show)
  df$varying_ <- factor(df$varying_, levels = vars_to_show)

  if (show_exposure > 0) {
    mult <- show_exposure * diff(range(df$value_, na.rm = TRUE)) / max(x$data$exposure)
    add <- min(df$value_, na.rm = TRUE)
    
    bars <- ggplot2::geom_tile(
      x$data, 
      mapping = ggplot2::aes(
        x = v,
        y = exposure / 2 * mult + add,
        height = exposure * mult,
        width = bar_width
      ), 
      show.legend = FALSE, 
      fill = fill
    )
    # ggplot2::scale_y_continuous(
    # sec.axis = ggplot2::sec_axis(
    #   transform = ~ (. - add) / mult, name = ggplot2::element_blank()
    # ))
  } else {
    bars <- NULL
  }
  
  p <- ggplot2::ggplot(df, aes(x = v, y = value_)) +
    bars +
    ggplot2::geom_point(ggplot2::aes(color = varying_), size = 2) +
    ggplot2::geom_line(
      ggplot2::aes(color = varying_, group = varying_), linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(values = line_colors) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "right") +
    ggplot2::labs(x = x$v, y = "Prediction scale")
  
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
#' @seealso See [marginal()] for examples.
hist2 <- function(x, breaks = 17L, trim = c(0.01, 0.99), right = TRUE, na.rm = TRUE) {
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
    xx, breaks = breaks, include.lowest = TRUE, right = right, plot = FALSE
  )
  b <- h$breaks
  ix <- findInterval(
    x, vec = b, left.open = right, rightmost.closed = TRUE, all.inside = TRUE
  )
  g <- h$mids
  if (!na.rm && anyNA(x)) {
    g <- c(g, NA)
  }
  list(x = g[ix], breaks = b, grid = g, discrete = FALSE)
}

fit <- ranger(price~ carat + color + cut + clarity, data = diamonds)

profvis::profvis(x <- marginal.default(
  fit,
  v = "color",
  X = diamonds,
  y = "price",
  pred_fun = pf,
  breaks = c(0.2, 0.4, 0.6, 0.8, 1, 1.5, 2, 3, 6),
  max_bins_discrete = 2
))
