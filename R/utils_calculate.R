#' Fast Index Generation
#' 
#' For not too small m much faster than `rep(seq_len(m), each = each)`.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param m Integer. See `each`.
#' @param each Integer. How many times should each value in `1:m` be repeated?
#' @returns Like `x`, but converted to matrix.
#' @examples
#' rep_each(10, 2)
#' rep(1:10, each = 2)  # Dito
rep_each <- function(m, each) {
  out <- .col(dim = c(each, m))
  dim(out) <- NULL
  out 
}

#' Fast OHE
#' 
#' Turns vector/factor into its One-Hot-Encoding.
#' Ingeniouly written by Mathias Ambuehl.
#' 
#' Working with integers instead of doubles would be slightly faster, but at the price
#' of potential integer overflows in subsequent calculations.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Object representing model predictions.
#' @returns Like `x`, but converted to matrix.
fdummy <- function(x) {
  x <- as.factor(x)
  lev <- levels(x)
  out <- matrix(0, nrow = length(x), ncol = length(lev))
  out[cbind(seq_along(x), as.integer(x))] <- 1
  colnames(out) <- lev
  out 
}

#' Weighted Version of colMeans()
#' 
#' Internal function used to calculate column-wise weighted means.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A vector or matrix-like.
#' @param w Optional case weights.
#' @returns A vector of column means.
wcolMeans <- function(x, w = NULL) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w)
}

#' Grouped Column Means
#' 
#' Internal workhorse to aggregate predictions over fixed-length groups.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Vector or matrix-like.
#' @param ngroups Number of groups (`x` was stacked that many times).
#' @param w Optional vector with case weights of length `NROW(x) / ngroups`.
#' @returns A (g x K) matrix, where g is the number of groups, and K = NCOL(x).
wrowmean <- function(x, ngroups = 1L, w = NULL) {
  if (ngroups == 1L) {
    return(t.default(wcolMeans(x, w = w)))
  }
  
  # Very fast case for 1d structures
  if (is.vector(x) || (is.matrix(x) && ncol(x) == 1L)) {
    return(wrowmean_vector(x, ngroups = ngroups, w = w))
  }
  wrowmean_matrix(x, ngroups = ngroups, w = w)
}

#' wrowmean() for Vectors
#'
#' Grouped means over fixed-length groups for vectors or 1d matrices.
#'
#' @noRd
#' @keywords internal
#'
#' @param x Vector or matrix with one column.
#' @param ngroups Number of subsequent, equals sized groups.
#' @param w Optional vector of case weights of length `NROW(x) / ngroups`.
#' @returns Matrix with one column.
wrowmean_vector <- function(x, ngroups = 1L, w = NULL) {
  nm <- if (is.matrix(x)) colnames(x)
  dim(x) <- c(length(x) %/% ngroups, ngroups)
  out <- wcolMeans(x, w = w)
  dim(out) <- c(ngroups, 1L)
  if (!is.null(nm)) {
    colnames(out) <- nm
  }
  out
}

#' wrowmean() for Matrices
#'
#' Grouped column means over fixed-length groups for matrices.
#'
#' @noRd
#' @keywords internal
#'
#' @param x Matrix-like.
#' @param ngroups Number of subsequent, equals sized groups.
#' @param w Optional vector of case weights of length `NROW(x) / ngroups`.
#' @returns Matrix.
wrowmean_matrix <- function(x, ngroups = 1L, w = NULL) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  n_bg <- nrow(x) %/% ngroups
  g <- rep_each(ngroups, each = n_bg)  # rep(seq_len(ngroups), each = n_bg)
  if (is.null(w)) {
    out <- rowsum(x, group = g, reorder = FALSE) / n_bg
  } else {
    # w is recycled over rows and columns
    out <- rowsum(x * w, group = g, reorder = FALSE) / sum(w)
  }
  rownames(out) <- NULL
  out
}

#' Grouped wcolMeans()
#' 
#' Internal function used to calculate grouped column-wise weighted means along with
#' corresponding (weighted) counts.
#'
#' @noRd
#' @keywords internal
#' 
#' @param x A vector or matrix-like object.
#' @param g Optional grouping variable.
#' @param w Optional case weights.
#' @returns A list with two elements: "M" represents a matrix of grouped (column) 
#'   means, and "w" is a vector of corresponding group counts/weights.
#' @examples 
#' with(iris, gwColMeans(Sepal.Width, g = Species, w = Sepal.Length))
gwColMeans <- function(x, g = NULL, w = NULL) {
  if (is.null(g)) {
    M <- t.default(wcolMeans(x, w = w))
    denom <- if (is.null(w)) NROW(x) else sum(w)
    return(list(M = M, w = denom))
  }
  
  # Now the interesting case
  if (!is.vector(x) && !is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(w)) {
    w <- rep.int(1, NROW(x))
  } else {
    x <- x * w  # w is correctly recycled over columns
  }
  denom <- as.numeric(rowsum(w, group = g))
  list(M = rowsum(x, group = g) / denom, w = denom)
}

#' Weighted Mean Centering
#' 
#' Internal function used to center each column of an object by subtracting its 
#' possibly weighted mean. Vector input is turned into a matrix with one column.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Vector or matrix-like.
#' @param w Optional vector of case weights.
#' @returns Centered version of `x` (vectors are turned into single-column matrix).
wcenter <- function(x, w = NULL) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  # sweep(x, MARGIN = 2L, STATS = wcolMeans(x, w = w))  # Slower
  x - matrix(wcolMeans(x, w = w), nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
}

#' Fast Row Subsetting (from kernelshap)
#' 
#' Internal function used to row-subset data.frames.
#' Brings a massive speed-up for data.frames. All other classes (tibble, data.table,
#' matrix) are subsetted in the usual way.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A matrix-like object.
#' @param i Logical or integer vector of rows to pick.
#' @returns Subsetted version of `x`.
rep_rows <- function(x, i) {
  if (!(all(class(x) == "data.frame"))) {
    return(x[i, , drop = FALSE])  # matrix, tibble, data.table, ...
  }
  # data.frame
  out <- lapply(x, function(z) if (length(dim(z)) != 2L) z[i] else z[i, , drop = FALSE])
  attr(out, "row.names") <- .set_row_names(length(i))
  class(out) <- "data.frame"
  out
}
