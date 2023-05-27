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

check_pred <- function(x) {
  if (!is.vector(x) && !is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) {
    stop("Predictions must be numeric")
  }
  x
}

fix_names <- function(out, out_names, prefix = "pred") {
  if (!is.null(out_names)) {
    colnames(out) <- out_names
  } else if (is.null(colnames(out))) {
    p <- ncol(out)
    colnames(out) <- if (p == 1L) prefix else paste(prefix, seq_len(p), sep = "_")
  }
  out
}

rowsum2 <- function(x, ngroups, w = NULL) {
  p <- NCOL(x)
  n_bg <- NROW(x) %/% ngroups
  g <- rep(seq_len(ngroups), each = n_bg)
  if (is.null(w)) {
    return(rowsum(x, group = g, reorder = FALSE) / n_bg)
  }
  # w is recycled over rows and columns
  rowsum(x * w, group = g, reorder = FALSE) / sum(w)
}
