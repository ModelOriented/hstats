#' Friedman's H
#' 
#' @inheritParams fx_pdp
#' @param v Vector of variables names to calculate Friedman's H for.
#' @param normalize TODO
#' @param take_sqrt TODO
#' @param verbose Should a progress bar be shown? Default is `TRUE`.
#' @returns A data.frame
#' @references
#'   Friedman J. H. (2001). Greedy function approximation: A gradient boosting machine.
#'   The Annals of Statistics, 29:1189–1232.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' fx_friedmans_h(fit, v = names(iris[-1]), X = iris)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' h <- fx_friedmans_h(fit, v = colnames(iris[3:5]), X = iris)
#' h
#' fx_friedmans_h(fit, v = c("Petal.Width", "Species"), X = iris)
fx_friedmans_h <- function(object, ...) {
  UseMethod("fx_friedmans_h")
}

#' @describeIn fx_friedmans_h Default method.
#' @export
fx_friedmans_h.default <- function(object, v, X, pred_fun = stats::predict, 
                                   normalize = TRUE, take_sqrt = TRUE,
                                   grid_size = 200L, n_max = 500L, out_names = NULL, 
                                   w = NULL, verbose = TRUE, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= c(2L, 2L),
    all(v %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X),
    length(v) >= 2L
  )
  
  n <- nrow(X)
  
  # Grid with all v
  grid <- if (n > grid_size) X[sample(n, grid_size), v] else X[, v]
  if (!is.data.frame(grid)) {
    grid <- as.data.frame(grid)
  }

  # Reduce size of X (and w)
  if (n > n_max) {
    ix <- sample(n, n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
    n <- n_max
  }
  
  # First, create univariate results
  pd1d <- mapply(
    pdp_raw,
    v = v, 
    grid = grid, 
    MoreArgs = list(object = object, X = X, pred_fun = pred_fun, w = w, ...),
    SIMPLIFY = FALSE
  )
  names(pd1d) <- v
  pd1d <- lapply(pd1d, .scale)
  
  # Then, loop over bivariate cases
  all_combs <- utils::combn(v, 2L, simplify = FALSE)
  ncomb <- length(all_combs)
  out <- vector("list", length = ncomb)
  show_bar <- verbose && ncomb > 1L
  if (show_bar) {
    pb <- utils::txtProgressBar(1L, ncomb, style = 3)
  }
  
  for (i in seq_len(ncomb)) {
    z <- all_combs[[i]]
    pd <- pdp_raw(
      object, v = z, X = X, pred_fun = pred_fun, grid = grid[, z], w = w, ...
    )
    pd <- .scale(pd)
    H <- colMeans((pd - pd1d[[z[1L]]] - pd1d[[z[2L]]])^2)
    if (normalize) {
      H <- H / colMeans(pd^2)
    }
    if (take_sqrt) {
      H <- sqrt(H)
    }

    if (show_bar) {
      utils::setTxtProgressBar(pb, i)
    }
    out[[i]] <- H
  }
  if (show_bar) {
    cat("\n")
  }
  
  # Organize output
  Combs <- stats::setNames(do.call(rbind, all_combs), c("V1", "V2"))
  Stats <- .zap_small(do.call(rbind, out))
  Stats <- fix_names(Stats, out_names = out_names, prefix = "Stat")
  data.frame(Combs, Stats)
}


#' #' @describeIn fx_friedmans_h Method for "ranger" models, see Readme for an example.
#' #' @export
#' fx_friedmans_h.ranger <- function(object, v, X, 
#'                            pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions, 
#'                            normalize = TRUE, take_sqrt = TRUE,
#'                            grid_size = 200L, n_max = 500L, out_names = NULL, 
#'                            w = NULL, verbose = TRUE, ...) {
#'   fx_friedmans_h.default(
#'     object = object,
#'     v = v,
#'     X = X,
#'     pred_fun = pred_fun,
#'     grid = grid,
#'     grid_size = grid_size,
#'     trim = trim,
#'     n_max = n_max,
#'     out_names = out_names,
#'     w = w,
#'     ...
#'   )
#' }
#' 
#' #' @describeIn fx_friedmans_h Method for "mlr3" models, see Readme for an example.
#' #' @export
#' fx_friedmans_h.Learner <- function(object, v, X, 
#'                             pred_fun = function(m, X) m$predict_newdata(X)$response, 
#'                             grid = NULL, grid_size = 36L, trim = c(0.01, 0.99), 
#'                             n_max = 500L, out_names = NULL, w = NULL, ...) {
#'   fx_friedmans_h.default(
#'     object = object,
#'     v = v,
#'     X = X,
#'     pred_fun = pred_fun,
#'     grid = grid,
#'     grid_size = grid_size,
#'     trim = trim,
#'     n_max = n_max,
#'     out_names = out_names,
#'     w = w,
#'     ...
#'   )
#' }


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
