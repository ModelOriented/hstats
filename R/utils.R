# Helper functions

# RLE with sorted values, also works for data.frames and matrices
rle2 <- function(X) {
  if (NCOL(X) == 1L) {
    X <- sort(X)
    dup <- duplicated(X)
    values <- X[!dup]
  } else {
    is_mat <- is.matrix(X)
    if (is_mat) {
      X <- as.data.frame(X)
    }
    X <- X[do.call(order, X), ]
    dup <- duplicated(X)
    values <- X[!dup, ]
    if (is_mat) {
      values <- as.matrix(values)
    }
  }
  # tabulate(...) =~rle(as.integer(interaction(X[!dup, ])))
  list(values = values, lengths = tabulate(cumsum(!dup)))
}

# Discretizes univariate vector z for most types of z
fixed_grid_one <- function(z, m = 36L, trim = c(0.01, 0.99)) {
  if (is.factor(z)) {
    return(levels(z))
  }
  uni <- unique(z)
  if (!is.numeric(z) || length(uni) <= m) {
    return(sort(uni))
  }
  
  # Non-discrete
  p <- seq(trim[1L], trim[2L], length.out = m)
  unique(stats::quantile(z, probs = p, names = FALSE, type = 1L))
}

# Keeps type of vv (vector/factor/matrix/data.frame). Note: if vv is matrix/df,
# it has at least two columns
make_grid <- function(vv, grid_type = "fixed", m = 36L, trim = c(0.01, 0.99)) {
  p <- NCOL(vv)
  if (grid_type == "random") {
    n <- NROW(vv)
    if (n <= m) {
      return(vv)
    }
    ix <- sample(n, size = m)
    if (p == 1L) {
      return(vv[ix])
    }
    return(vv[ix, ])
  }
  
  # grid_type == "fixed"
  if (p == 1L) {
    return(fixed_grid_one(vv, m = m, trim = trim))
  }
  m <- ceiling(m^(1/p))  # take p's root of m
  is_mat <- is.matrix(vv)
  if (is_mat) {
    vv <- as.data.frame(vv)
  }
  out <- expand.grid(lapply(vv, FUN = fixed_grid_one, m = m, trim = trim))
  if (is_mat) {
    return(as.matrix(out))
  }
  out
}

# Check consistency of grid and v, drop unnecessary dimension
check_grid <- function(g, v) {
  if (length(v) != NCOL(grid)) {
    stop("NCOL(grid) must equal length(v)")
  }
  if (length(v) == 1L) {
    if (!is.vector(g) && !is.factor(g)) {
      stop("'grid' should be a vector of values")
    }
  } else if (is.null(colnames(g)) || !all(v == colnames(g))) {
      stop("Column names of 'grid' unequal to v")
  }
  TRUE
}

# Turns predictions into unnamed vectors (if 1D) or matrices without rownames otherwise
check_pred <- function(x) {
  if (!is.vector(x) && !is.matrix(x) && !is.data.frame(x) && !is.array(x)) {
    stop("Predictions must be a vector, matrix, data.frame, or array")
  }
  if (!is.vector(x)) {
    if (NCOL(x) == 1L) {
      if (is.data.frame(x)) {
        x <- x[[1L]]
      } else {
        x <- c(x)
      }
    } else {
      if (is.data.frame(x) || is.array(x)) {
        x <- as.matrix(x)
      }
    }
  }
  if (!is.numeric(x)) {
    stop("Predictions must be numeric")
  }
  x
}


# removes rownames from vector, matrix, data.frame
Unname <- function(z) {
  if (is.vector(z)) {
    return(unname(z))
  }
  rownames(z) <- NULL
  z
}
