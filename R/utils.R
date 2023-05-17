# Helper functions

# RLE with sorted values, also works for data.frames and matrices
rle2 <- function(X) {
  if (NCOL(X) == 1L && !is.data.frame(X)) {
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
fixed_grid_one <- function(z, m = 27L, trim = c(0.01, 0.99)) {
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

# vv is either a vector (if length(v) == 1) or a matrix/data.frame with >2 cols
make_grid <- function(vv, grid_type = "fixed", m = 27L, trim = c(0.01, 0.99)) {
  if (grid_type == "random") {
    n <- nrow(vv)
    if (n <= m) {
      return(vv)
    }
    ix <- sample(n, size = m)
    if (NCOL(vv) == 1L) {
      return(vv[ix])
    }
    return(vv[ix, ])
  }
  
  # grid_type == "fixed"
  if (NCOL(vv) == 1L) {
    return(fixed_grid_one(vv, m = m, trim = trim))
  } 
  if (is.matrix(vv)) {
    vv <- as.data.frame(vv)
  }
  expand.grid(lapply(vv, FUN = fixed_grid_one, m = m, trim = trim))
}

# removes rownames from vector, matrix, data.frame
Unname <- function(z) {
  if (is.vector(z)) {
    return(unname(z))
  }
  rownames(z) <- NULL
  z
}
