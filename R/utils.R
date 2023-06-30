# Fix undefined global variable note
utils::globalVariables(c("varying_", "value_", "id_", "variable_"))

#' Aligns Predictions
#' 
#' Turns predictions into matrix.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Object representing model predictions.
#' @returns Like `x`, but converted to matrix.
align_pred <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) {
    stop("Predictions must be numeric")
  }
  x
}

#' Fast Weighted Mean by Fixed-Length Groups
#' 
#' Internal workhorse to aggregate predictions per evaluation point of a PD.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Vector or matrix.
#' @param ngroups Number of groups of fixed length `NROW(x) / ngroups`.
#' @param w Optional vector with case weights of length `NROW(x) / ngroups`.
#' @returns A (g x K) matrix, where g is the grid size, and K = NCOL(x).
wrowmean <- function(x, ngroups, w = NULL) {
  p <- NCOL(x)
  n_bg <- NROW(x) %/% ngroups
  g <- rep(seq_len(ngroups), each = n_bg)
  # Even faster: cbind(collapse::fmean(x, g = g, w = w))
  if (is.null(w)) {
    out <- rowsum(x, group = g, reorder = FALSE) / n_bg
  } else {
    if (length(w) != n_bg) {
      stop("w must be of length NROW(x) / ngroups.")
    }
    # w is recycled over rows and columns
    out <- rowsum(x * w, group = g, reorder = FALSE) / sum(w)
  }
  rownames(out) <- NULL
  out
}

#' Compresses X
#' 
#' @description
#' Internal function to remove duplicated rows in `X` based on columns not in `v`. 
#' Compensation is done by summing corresponding case weights `w`. 
#' Currently implemented only for the case when there is a single non-`v` column in `X`.
#' Can later be generalized to multiple columns via [paste()]. 
#' 
#' Notes:
#' - This function is important for interaction calculations.
#' - The initial check for having a single non-`v` column is very cheap.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams pd_raw
#' @returns A list with `X` and `w`, potentially compressed.
.compress_X <- function(X, v, w = NULL) {
  not_v <- setdiff(colnames(X), v)
  if (length(not_v) != 1L) {
    return(list(X = X, w = w))  # No optimization implemented for this case
  }
  x_not_v <- if (is.data.frame(X)) X[[not_v]] else X[, not_v]
  X_dup <- duplicated(x_not_v)
  if (!any(X_dup)) {
    return(list(X = X, w = w))  # No optimization done
  }

  # Compress
  if (is.null(w)) {
    w <- rep(1.0, times = nrow(X))
  }
  list(
    X = X[!X_dup, , drop = FALSE], 
    w = c(rowsum(w, group = x_not_v, reorder = FALSE))
  )
}

#' Compresses Grid
#' 
#' Internal function used to remove duplicated grid rows. Re-indexing to original grid 
#' rows needs to be later, but this function provides the re-index vector to do so.
#' Further note that checking for uniqueness can be costly for higher-dimensional grids.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams pd_raw
#' @returns 
#'   A list with `grid` (possibly compressed) and the optional `reindex` vector
#'   used to map compressed grid values back to the original grid rows. The original
#'   grid equals the compressed grid at indices `reindex`.
.compress_grid <- function(grid) {
  ugrid <- unique(grid)
  if (NROW(ugrid) == NROW(grid)) {
    # No optimization done
    return(list(grid = grid, reindex = NULL))
  }
  out <- list(grid = ugrid)
  if (NCOL(grid) >= 2L) {  # Non-vector case
    grid <- apply(grid, MARGIN = 1L, FUN = paste, collapse = "_:_")
    ugrid <- apply(ugrid, MARGIN = 1L, FUN = paste, collapse = "_:_")
    if (anyDuplicated(ugrid)) {
      stop("String '_:_' found in grid values at unlucky position.")
    }
  }
  out[["reindex"]] <- match(grid, ugrid)
  out
}

#' Weighted Version of colMeans()
#' 
#' Internal function used to calculate column-wise weighted means.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A matrix-like object.
#' @param w Optional case weights.
#' @returns A vector of column means.
wcolMeans <- function(x, w = NULL) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w) 
}

#' Weighted Mean Centering
#' 
#' Internal function used to center each column of an object by subtracting its 
#' possibly weighted mean. Vector input is turned into a matrix with one column.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Matrix, data.frame, or vector.
#' @param w Optional vector of case weights.
#' @returns Centered version of `x` (vectors are turned into single-column matrix).
wcenter <- function(x, w = NULL) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  # sweep(x, MARGIN = 2L, STATS = wcolMeans(x, w = w))  # Slower
  x - matrix(wcolMeans(x, w = w), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
}

#' Basic Checks
#' 
#' Internal function used to check some basic things.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams pd_raw
#' @returns Error or TRUE
basic_check <- function(X, v, pred_fun, w) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= c(1L, 1L),
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X)
  )
  TRUE
}

#' Postprocessing of Statistics
#' 
#' Function to apply typical postprocessing steps to a Friedman-Popescu type statistic.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams H2_j
#' @param num Matrix or vector of statistic.
#' @param denom Denominator of statistic (a matrix or vector compatible with `num`).
#' @returns Matrix or vector of statistics.
postprocess <- function(num, denom = 1, normalize = TRUE, squared = TRUE, 
                        sort = TRUE, top_m = Inf, eps = 1e-8) {
  out <- .zap_small(num, eps = eps)
  if (normalize) {
    out <- out / denom
  }
  if (!squared) {
    out <- sqrt(out)
  }
  if (sort) {
    if (is.matrix(out)) {
      out <- out[order(-rowSums(out)), , drop = FALSE]
    } else {
      out <- sort(out, decreasing = TRUE)
    }
  }
  utils::head(out, n = top_m)
}

#' Zap Small Values
#' 
#' Internal function. Sets very small or non-finite (NA, ...) values in vector, 
#' matrix or data.frame to 0.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Vector, matrix, or data.frame.
#' @param eps Threshold, below which absolute values are set to 0.
#' @returns Same as `x` but with values below `eps` replaced by 0.
.zap_small <- function(x, eps = 1e-8) {
  zero <- abs(x) < eps | !is.finite(x)
  if (any(zero)) {
    x[zero] <- 0
  }
  x
}

#' Stack some Columns
#' 
#' Internal function used in the plot method for "pd" objects. The function brings
#' wide columns `to_stack` (the prediction dimensions) into long form.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param data A data.frame.
#' @param to_stack Column names in `data` to bring from wide to long form.
#' @returns 
#'   A data.frame with variables not in `to_stack`, a column "varying_" with
#'   the column name from `to_stack`, and finally a column "value_" with stacked values.
poor_man_stack <- function(data, to_stack) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  keep <- setdiff(colnames(data), to_stack)
  out <- lapply(
    to_stack, 
    FUN = function(z) cbind.data.frame(data[keep], varying_ = z, value_ = data[, z])
  )
  do.call(rbind, out)
}

#' Matrix to DF
#' 
#' Internal function used in the plot method for "interact" objects. It turns
#' matrix objects into (long) data.frames.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param mat A matrix.
#' @param id Value of column to be added as "id_".
#' @returns A data.frame.
mat2df <- function(mat, id = "Overall") {
  if (!is.matrix(mat)) {
    stop("'mat' must be a matrix.")
  }
  pred_names <- colnames(mat)
  K <- ncol(mat)
  nm <- rownames(mat)
  if (is.null(pred_names)) {
    pred_names <- if (K == 1L) "y" else paste0("y", seq_len(K))
    colnames(mat) <- pred_names
  }
  out <- cbind.data.frame(id_ = id, variable_ = factor(nm, levels = rev(nm)), mat)
  poor_man_stack(out, to_stack = pred_names)
}

#' Bin into Quantiles
#' 
#' Internal function. Applies [cut()] to quantile breaks.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A numeric vector.
#' @param m Number of intervals.
#' @returns A factor, representing binned `x`.
qcut <- function(x, m) {
  p <- seq(0, 1, length.out = m + 1L)
  g <- stats::quantile(x, probs = p, names = FALSE, type = 1L, na.rm = TRUE)
  cut(x, breaks = unique(g), include.lowest = TRUE)
}

#' Plots Matrix of Statistics
#' 
#' @noRd
#' @keywords internal
#'
#' @param x A matrix of statistics with rownames.
#' @param fill Color of bar (only for univariate statistics).
#' @param ... Arguments passed to `geom_bar()`.
#' @returns An object of class "ggplot".
plot_stat <- function(x, fill = "#2b51a1", ...) {
  p <- ggplot2::ggplot(mat2df(x), ggplot2::aes(x = value_, y = variable_)) +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab("Value")
  
  if (ncol(x) == 1L) {
    p + ggplot2::geom_bar(fill = fill, stat = "identity", ...)
  } else {
    p + 
      ggplot2::geom_bar(
        ggplot2::aes(fill = varying_), stat = "identity", position = "dodge", ...
      ) + 
      ggplot2::labs(fill = "Response")
  }
}

#' Plots PD
#' 
#' Plots partial dependencies. It supports multivariate predictions and a BY variable, 
#' but only univariable `v`.
#' 
#' @noRd
#' @keywords internal
#' 
#' @importFrom ggplot2 .data
#' @inheritParams PDP
#' @param x A data.frame with grid, the optional BY variable and partial dependencies.
#' @param ... Arguments passed to geometries.
#' @returns An object of class "ggplot".
plot_pd <- function(x, v, pred_names, BY = NULL, rotate_x = FALSE, 
                    color = "#2b51a1", facet_scales = "free_y", ...) {
  stopifnot(length(v) == 1L)
  data <- poor_man_stack(x, to_stack = pred_names)
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[v]], y = value_))
  if (is.null(BY)) {
    p <- p + 
      ggplot2::geom_line(color = color, group = 1, ...) +
      ggplot2::geom_point(color = color, ...)
  } else {
    p <- p + 
      ggplot2::geom_line(ggplot2::aes(color = .data[[BY]], group = .data[[BY]])) +
      ggplot2::geom_point(ggplot2::aes(color = .data[[BY]], group = .data[[BY]]))
  }
  
  if (length(pred_names) > 1L) {
    p <- p + ggplot2::facet_wrap(~ varying_, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )  
  }
  p + ggplot2::labs(x = v, y = "PD", color = BY)
}
