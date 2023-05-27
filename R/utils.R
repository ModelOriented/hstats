# Helper functions

# Discretizes univariate vector z for most types of z
fixed_grid_one <- function(z, m = 36L, trim = c(0.01, 0.99)) {
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
fixed_grid <- function(vv, m = 36L, trim = c(0.01, 0.99)) {
  p <- NCOL(vv)
  if (p == 1L) {
    if (is.data.frame(vv)) {
      vv <- vv[[1L]]
    }
    return(fixed_grid_one(vv, m = m, trim = trim))
  }
  m <- ceiling(m^(1/p))  # take p's root of m
  is_mat <- is.matrix(vv)
  if (is_mat) {
    vv <- as.data.frame(vv)
  }
  out <- expand.grid(lapply(vv, FUN = fixed_grid_one, m = m, trim = trim))
  if (is_mat) as.matrix(out) else out
}

# Check consistency of grid and v
check_grid <- function(g, v, X_is_matrix) {
  p <- length(v)
  if (p != NCOL(g)) {
    stop("NCOL(grid) must equal length(v)")
  }
  if (p == 1L) {
    if (!is.vector(g) && !is.factor(g)) {
      stop("'grid' should be a vector of values")
    }
  } else {
    stopifnot(
      is.matrix(g) || is.data.frame(g),
      is.matrix(g) == X_is_matrix,
      !is.null(colnames(g)),
      all(v == colnames(g))
    )
  }
  TRUE
}

pred2matrix <- function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) {
    stop("Predictions must be numeric")
  }
  x
}

fix_pd_names <- function(pd, pd_names) {
  if (!is.null(pd_names)) {
    colnames(pd) <- pd_names
  } else if (is.null(colnames(pd))) {
    p <- ncol(pd)
    colnames(pd) <- if (p == 1L) "pred" else paste("pred", seq_len(p), sep = "_")
  }
  pd
}

