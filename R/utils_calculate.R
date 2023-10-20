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
#' @param ngroups Number of groups (`x` was stacked that many times).
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

#' Approximate Vector
#' 
#' Internal function. Approximates values by the average of the two closest quantiles.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A vector or factor.
#' @param m Number of unique values.
#' @returns An approximation of `x` (or `x` if non-numeric or discrete).
approx_vector <- function(x, m = 50L) {
  if (!is.numeric(x) || length(unique(x)) <= m) {
    return(x)
  }
  p <- seq(0, 1, length.out = m + 1L)
  q <- unique(stats::quantile(x, probs = p, names = FALSE, na.rm = TRUE))
  mids <- (q[-length(q)] + q[-1L]) / 2
  return(mids[findInterval(x, q, rightmost.closed = TRUE)])
}

#' Approximate df or Matrix
#' 
#' Internal function. Calls `approx_vector()` to each column in matrix or data.frame.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param X A matrix or data.frame.
#' @param m Number of unique values.
#' @returns An approximation of `X` (or `X` if non-numeric or discrete).
approx_matrix_or_df <- function(X, v = colnames(X), m = 50L) {
  stopifnot(
    m >= 2L,
    is.data.frame(X) || is.matrix(X)
  )
  if (is.data.frame(X)) {
    X[v] <- lapply(X[v], FUN = approx_vector, m = m)  
  } else {  # Matrix
    X[, v] <- apply(X[, v, drop = FALSE], MARGIN = 2L, FUN = approx_vector, m = m)  
  }
  return(X)
}
