#' Friedman's H
#' 
#' @inheritParams fx_pdp
#' @param v Vector of feature names for which interaction statistics are to be crunched.
#' @param normalize Should explained variances be normalized? The default is `TRUE`.
#'   If `squared = FALSE`, this produces Friedman's H.
#' @param squared Should squared statistics be returned? The default is `FALSE`. 
#'   If `normalize = TRUE`, this produces Friedman's H.
#' @param verbose Should a progress bar be shown? Default is `TRUE`.
#' @returns A data.frame
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'   The Annals of Statistics, 29:1189–1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' fx_friedmans_h(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' fx_friedmans_h(fit, v = names(iris[-1]), X = iris, verbose = FALSE, pairwise = TRUE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' fx_friedmans_h(fit, v = v, X = iris)
#' fx_friedmans_h(fit, v = v, X = iris, pairwise = TRUE)
#' 
#' # MODEL THREE: Gamma GLM with log link
#' fit <- glm(
#'   Sepal.Length ~ . + Petal.Width:Species, 
#'   data = iris, 
#'   family = Gamma(link = log)
#' )
#' 
#' # No interactions for additive features, at least on link scale
#' fx_friedmans_h(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # On original scale, we have interactions everywhere...
#' fx_friedmans_h(fit, v = names(iris[-1]), X = iris, verbose = FALSE, type = "response")
fx_friedmans_h <- function(object, ...) {
  UseMethod("fx_friedmans_h")
}

#' @describeIn fx_friedmans_h Default method.
#' @export
fx_friedmans_h.default <- function(object, v, X, pred_fun = stats::predict,
                                   pairwise = FALSE, normalize = TRUE, squared = FALSE,
                                   n_max = 200L, out_names = NULL, 
                                   w = NULL, verbose = TRUE, ...) {
  p <- length(v)
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= c(2L, p),
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X),
    p >= 1L + pairwise
  )
  
  # Reduce size of X (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
  }
  
  # Initialize progress bar
  if (verbose) {
    J <- p + if (pairwise) p * (p - 1) / 2 else p
    j <- 1L
    pb <- utils::txtProgressBar(1L, J, style = 3)
  }
  
  # Univariate PDs (required for both pairwise TRUE/FALSE)
  pd1d <- stats::setNames(vector("list", length = length(v)), v)
  for (z in v) {
    g <- if (is.data.frame(X)) X[[z]] else X[, z]
    pd1d[[z]] <- .scale(
      pdp_raw(object = object, v = z, X = X, pred_fun = pred_fun, grid = g, w = w, ...)
    )
    if (verbose) {
      utils::setTxtProgressBar(pb, j)
      j <- j + 1L
    }
  }

  # Calculate predictions over X for non-pairwise case (very cheap)
  if (!pairwise) {
    f <- check_pred(pred_fun(object, X, ...))
    if (!is.matrix(f)) {
      f <- as.matrix(f)
    }
    f <- .scale(f)
  }
  
  # Prepare loop index and corresponding output structure
  if (pairwise) {
    combs <- utils::combn(v, 2L, simplify = FALSE)
    V <- stats::setNames(as.data.frame(do.call(rbind, combs)), c("V1", "V2"))
  } else {
    V <- data.frame(V = v)
  }
  m <- nrow(V)
  H_denom <- H_num <- vector("list", length = m)
  
  for (i in seq_len(m)) {
    if (pairwise) {
      z <- combs[[i]]
      f <- .scale(
        pdp_raw(object, v = z, X = X, pred_fun = pred_fun, grid = X[, z], w = w, ...)
      )
      pd_i <- pd1d[[z[1L]]]
      pd_j <- pd1d[[z[2L]]]
    } else {
      z <- v[i]
      not_z <- setdiff(colnames(X), z) 
      pd_j <- .scale(
        pdp_raw(
          object, v = not_z, X = X, pred_fun = pred_fun, grid = X[, not_z], w = w, ...
        )
      )
      pd_i <- pd1d[[z]]
    }
    H_num[[i]] <- colMeans((f - pd_i - pd_j)^2)
    H_denom[[i]] <- colMeans(f^2)
    
    if (verbose) {
      utils::setTxtProgressBar(pb, j)
      j <- j + 1L
    }
  }
  if (verbose) {
    cat("\n")
  }
  
  S <- .zap_small(do.call(rbind, H_num))
  H_denom <- do.call(rbind, H_denom)
  
  if (normalize) {
    S <- S / H_denom
  }
  if (!squared) {
    S <- sqrt(S)
  }
  S <- fix_names(S, out_names = out_names, prefix = "Stat")
  data.frame(V, S)
}


#' @describeIn fx_friedmans_h Method for "ranger" models, see Readme for an example.
#' @export
fx_friedmans_h.ranger <- function(object, v, X,
                                  pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                  pairwise = FALSE, normalize = TRUE, squared = FALSE,
                                  n_max = 200L, out_names = NULL, 
                                  w = NULL, verbose = TRUE, ...) {
  fx_friedmans_h.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise = pairwise,
    normalize = normalize,
    squared = squared,
    n_max = n_max,
    out_names = out_names,
    w = w,
    verbose = verbose,
    ...
  )
}

#' @describeIn fx_friedmans_h Method for "mlr3" models, see Readme for an example.
#' @export
fx_friedmans_h.Learner <- function(object, v, X,
                                   pred_fun = function(m, X) m$predict_newdata(X)$response,
                                   pairwise = FALSE, normalize = TRUE, squared = FALSE,
                                   n_max = 200L, out_names = NULL, 
                                   w = NULL, verbose = TRUE, ...) {
  fx_friedmans_h.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise = pairwise,
    normalize = normalize,
    squared = squared,
    n_max = n_max,
    out_names = out_names,
    w = w,
    verbose = verbose,
    ...
  )
}


# Helper

# Helper function used to clip small values.
.zap_small <- function(x, eps = 1e-8) {
  zero <- abs(x) < eps | !is.finite(x)
  if (any(zero)) {
    x[zero] <- 0
  }
  x
}

.scale <- function(x) {
  sweep(x, MARGIN = 2L, STATS = colMeans(x))
}
