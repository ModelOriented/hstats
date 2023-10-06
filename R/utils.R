# Fix undefined global variable note
utils::globalVariables(c("varying_", "value_", "id_", "variable_", "obs_", "error_"))

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
#' @returns A (g x K) matrix, where g is the number of groups, and K = NCOL(x).
wrowmean <- function(x, ngroups = 1L, w = NULL) {
  if (ngroups == 1L) {
    return(rbind(wcolMeans(x, w = w)))
  }
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
  if (NCOL(x) == 1L && is.null(w)) {
    return(mean(x))
  }
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w) 
}

#' Grouped wcolMeans()
#' 
#' Internal function used to calculate grouped column-wise weighted means.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A matrix-like object.
#' @param g Optional grouping variable.
#' @param w Optional case weights.
#' @param reorder Should groups be ordered, see [rowsum()]. Default is `TRUE`. 
#' @returns A matrix with one row per group.
gwColMeans <- function(x, g = NULL, w = NULL, reorder = TRUE) {
  if (is.null(g)) {
    return(rbind(wcolMeans(x, w = w)))
  }
  if (is.null(w)) {
    num <- rowsum(x, group = g, reorder = reorder)
    denom <- rowsum(rep.int(1, NROW(x)), group = g, reorder = reorder)
  } else {
    num <- rowsum(x * w, group = g, reorder = reorder)
    denom <- rowsum(w, group = g, reorder = reorder)
  }
  num / matrix(denom, nrow = nrow(num), ncol = ncol(num), byrow = FALSE)
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
basic_check <- function(X, v, pred_fun, w = NULL) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= c(1L, 1L),
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X)
  )
  TRUE
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
#' @returns A data.frame, or `NULL`.
mat2df <- function(mat, id = "Overall") {
  if (is.null(mat)) {
    return(NULL)
  }
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
  if (is.null(nm)) {
    nm <- seq_along(nrow(mat))
  }
  out <- cbind.data.frame(id_ = id, variable_ = factor(nm, levels = rev(nm)), mat)
  poor_man_stack(out, to_stack = pred_names)
}

#' Initializer of Numerator Statistics
#' 
#' Internal helper function that returns a matrix of all zeros with the right
#' column and row names for statistics of any "way". If some features have been dropped
#' from the statistics calculations, they are added as 0.
#' 
#' @noRd
#' @keywords internal
#' @param x A list containing the elements "v", "K", "pred_names", "v_pairwise", 
#'   "v_threeway", "pairwise_m", "threeway_m".
#' @param way Integer between 1 and 3 of the order of the interaction.
#' @returns A matrix of all zeros.
init_numerator <- function(x, way = 1L) {
  stopifnot(way %in% 1:3)
  
  v <- x[["v"]]
  K <- x[["K"]]
  pred_names <- x[["pred_names"]]
  
  # Simple case
  if (way == 1L) {
    return(matrix(nrow = length(v), ncol = K, dimnames = list(v, pred_names)))
  }
  
  # Determine v_cand_0, which is v_cand with additional features to end up with length m
  if (way == 2L) {
    v_cand <- x[["v_pairwise"]]
    m <- x[["pairwise_m"]]
  } else {
    v_cand <- x[["v_threeway"]]
    m <- x[["threeway_m"]]
  }
  m_miss <- m - length(v_cand)
  if (m_miss > 0L) {
    v_cand_0 <- c(v_cand, utils::head(setdiff(v, v_cand), m_miss))
    v_cand_0 <- v[v %in% v_cand_0]  # Bring into order of v
  } else {
    v_cand_0 <- v_cand
  }
  
  # Get all interactions of order "way". c() turns the array into a vector
  cn0 <- c(utils::combn(v_cand_0, m = way, FUN = paste, collapse = ":"))
  matrix(0, nrow = length(cn0), ncol = K, dimnames = list(cn0, pred_names))
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

#' Prepares Group BY Variable
#' 
#' Internal function that prepares a BY variable or BY column name.
#' 
#' @noRd
#' @keywords internal
#' @param BY Vector/factor or column name in `X`.
#' @param X Matrix-like.
#' @param by_size Determines if numeric `X` is discrete or needs to be binned.
#' 
#' @returns A list.
prepare_by <- function(BY, X, by_size) {
  if (length(BY) == 1L && BY %in% colnames(X)) {
    by_name <- BY
    BY <- X[, by_name]
  } else {
    stopifnot(
      NCOL(BY) == 1L,
      is.vector(BY) || is.factor(BY),
      length(BY) == nrow(X)
    )
    by_name <- "Group"
  }
  
  # Binning
  by_values <- unique(BY)
  if (is.numeric(BY) && length(by_values) > by_size) {
    BY <- qcut(BY, m = by_size)
    by_values <- unique(BY)
  }
  list(BY = BY, by_values = by_values, by_name = by_name)
}

#' Rotate x labels in plots
#' 
#' @noRd
#' @keywords internal
#' 
#' @returns A theme object.
rotate_x_labs <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  )
}

#' Color Scale based on column
#' 
#' @noRd
#' @keywords internal
#' @param x A vector/factor.
#' 
#' @returns Discrete or continuous viridis color scale
get_color_scale <- function(x) {
  disc <- is.factor(x) || is.character(x) || is.logical(x)
  if (disc) ggplot2::scale_color_viridis_d else ggplot2::scale_color_viridis_c
}

#' mlr3 Helper
#' 
#' Returns the prediction function of a mlr3 Learner.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param object Learner object.
#' @param X Dataframe like object.
#' 
#' @returns A function.
mlr3_pred_fun <- function(object, X) {
  if ("classif" %in% object$task_type) {
    # Check if probabilities are available
    test_pred <- object$predict_newdata(utils::head(X))
    if ("prob" %in% test_pred$predict_types) {
      return(function(m, X) m$predict_newdata(X)$prob)
    } else {
      stop("Set lrn(..., predict_type = 'prob') to allow for probabilistic classification.")
    }
  }
  function(m, X) m$predict_newdata(X)$response
}
