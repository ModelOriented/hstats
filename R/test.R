library(microbenchmark)
library(collapse)


rowsum2 <- function(x, ngroups, w = NULL) {
  p <- NCOL(x)
  n <- NROW(x) %/% ngroups
  out <- matrix(nrow = ngroups, ncol = p)
  for (i in seq_len(ngroups)) {
    J <- (i - 1L) * n + 1:n
    if (p == 1L) {
      out[i, ] <- mean(x[J])
    } else {
      out[i, ] <- colMeans(x[J, ])
    }
  }
  out
}

rowsum3 <- function(x, ngroups, w = NULL) {
  p <- NCOL(x)
  n_bg <- NROW(x) %/% ngroups
  g <- rep(seq_len(ngroups), each = n_bg)
  if (is.null(w)) {
    return(rowsum(x, group = g, reorder = FALSE) / n_bg)
  }
  rowsum(x * rep(w, times = ngroups), group = g, reorder = FALSE) / sum(w)
}

# univariate
n <- 1000
ngroups <- 50
x <- rnorm(n * ngroups)
g <- rep(1:50, each = n)
w <- rep(runif(1000), times = ngroups)

microbenchmark(rowsum(x, g, reorder = FALSE) / n, times = 1000) # 0.8 ms
microbenchmark(rowsum2(x, 50), times = 1000) # 1 ms
microbenchmark(rowsum3(x, ngroups = 50, w = w), times = 1000) # 1.9 ms
microbenchmark(fmean(x, g, w = w), times = 1000) # 0.4 ms

# bivariate
n <- 1000
ngroups <- 50
x <- matrix(rnorm(n * ngroups * 4), ncol = 4)
g <- rep(1:50, each = n)

microbenchmark(rowsum(x, g, reorder = FALSE) / n, times = 1000) # 1.2 ms
microbenchmark(rowsum2(x, 50), times = 1000) # 1.7 ms
microbenchmark(fmean(x, g), times = 1000) # 0.8 ms

pdp_raw2 <- function(object, v, X, pred_fun, grid, w = NULL, ...) {
  n <- nrow(X)
  n_grid <- NROW(grid)
  D1 <- length(v) == 1L
  
  # Explode everything to n * n_grid rows
  X_pred <- X[rep(seq_len(n), times = n_grid), , drop = FALSE]
  if (D1) {
    grid_pred <- rep(grid, each = n)
  } else {
    grid_pred <- grid[rep(seq_len(n_grid), each = n), ]
  }
  if (!is.null(w)) {
    w <- rep(w, times = n_grid)
  }
  
  # Vary v
  if (D1 && is.data.frame(X_pred)) {
    X_pred[[v]] <- grid_pred  #  [, v] <- much slower if df
  } else {
    X_pred[, v] <- grid_pred
  }
  
  # Create **matrix** of predictions
  pred <- pred2matrix(pred_fun(object, X_pred, ...))
  pd <- rowsum3(pred, ngroups = n_grid, w = w)
  rownames(pd) <- NULL
  pd
}

