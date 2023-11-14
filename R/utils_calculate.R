#' Fast Index Generation
#' 
#' For not too small m, much faster than `rep(seq_len(m), each = each)`.
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
#' @param x A vector, factor, or matrix-like.
#' @param w Optional case weights.
#' @returns A vector of column means.
wcolMeans <- function(x, w = NULL) {
  if (is.factor(x)) {
    if (is.null(w)) {
      return(colMeans_factor(x))
    }
    x <- fdummy(x)
  } else if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(w)) {
    return(colMeans(x))
  }
  if (length(w) != nrow(x)) {
    stop("w must be of length nrow(x).")
  }
  if (!is.double(w)) {
    w <- as.double(w)
  }
  colSums(x * w) / sum(w)
}

#' colMeans() for Factors
#'
#' Internal function used to calculate proportions of factor levels.
#' It is less memory-hungry than `colMeans(fdummy(x))`, and much faster.
#' A weighted version via `rowsum(w, x)` is not consistently faster than
#' `wcolMeans(fdummy(x))`, so we currently focus on the non-weighted case.
#' Furthermore, `rowsum()` drops empty factor levels.
#'
#' @noRd
#' @keywords internal
#'
#' @param x Factor-like.
#' @returns Named vector.
colMeans_factor <- function(x) {
  x <- as.factor(x)
  lev <- levels(x)
  out <- tabulate(x, nbins = length(lev)) / length(x)
  names(out) <- lev
  out
}

#' Grouped Column Means
#' 
#' Internal workhorse to aggregate predictions over fixed-length groups.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Vector, factor or matrix-like.
#' @param ngroups Number of groups (`x` was stacked that many times).
#' @param w Optional vector with case weights of length `NROW(x) / ngroups`.
#' @returns A (g x K) matrix, where g is the number of groups, and K = NCOL(x).
wrowmean <- function(x, ngroups = 1L, w = NULL) {
  if (ngroups == 1L) {
    return(t.default(wcolMeans(x, w = w)))
  }
  
  # Prep w
  if (!is.null(w)) {
    if (length(w) != NROW(x) %/% ngroups) {
      stop("w must be of length NROW(x) / ngroups.")
    }
    if (!is.double(w)) {
      w <- as.double(w)
    }
  }
  
  # Very fast case for factors w/o weights and vectors/1d-matrices
  if (is.factor(x)) {
    if (is.null(w)) {
      return(rowmean_factor(x, ngroups = ngroups))
    }
    x <- fdummy(x)
  } 
  if (is.vector(x) || (is.matrix(x) && ncol(x) == 1L)) {
    return(wrowmean_vector(x, ngroups = ngroups, w = w))
  }

  # General version
  wrowmean_matrix(x, ngroups = ngroups, w = w)
}

#' (w)rowmean() for Factors (without weights)
#'
#' Grouped `colMeans_factor()` for equal sized groups.
#'
#' @noRd
#' @keywords internal
#'
#' @param x Factor-like.
#' @param ngroups Number of subsequent, equals sized groups.
#' @returns Matrix with column names.
rowmean_factor <- function(x, ngroups = 1L) {
  x <- as.factor(x)
  lev <- levels(x)
  n_bg <- length(x) %/% ngroups
  dim(x) <- c(n_bg, ngroups)
  out <- t.default(apply(x, 2L, FUN = tabulate, nbins = length(lev)))
  colnames(out) <- lev
  out / n_bg
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
#' @returns Matrix.
wrowmean_vector <- function(x, ngroups = 1L, w = NULL) {
  if (!(is.vector(x) || (is.matrix(x) && ncol(x) == 1L))) {
    stop("x must be a vector or a single column matrix")
  }
  nm <- if (is.matrix(x)) colnames(x)
  dim(x) <- c(length(x) %/% ngroups, ngroups)
  out <- as.matrix(if (is.null(w)) colMeans(x) else colSums(x * w) / sum(w))
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
  # Even faster: cbind(collapse::fmean(x, g = g, w = w))
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
#' @param x A vector, factor or matrix-like object.
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
  if (is.factor(x)) {
    x <- fdummy(x)
  } else if (!is.vector(x) && !is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (is.null(w)) {
    w <- rep.int(1, NROW(x))
  } else {
    if (!is.double(w)) {
      w <- as.double(w)
    }
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
