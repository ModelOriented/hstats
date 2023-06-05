#' Fast Friedman's H
#'  
#' @inheritParams fx_pdp
#' @param v Vector of feature names for which interaction statistics are to be 
#'   calculated.
#' @param pairwise The default (`FALSE`) calculates overall interaction strength per 
#'   feature. Set to `TRUE` to get *pairwise* statistics (slower).
#' @returns 
#'   An object of class "fx_interaction", containing these elements:
#'   - `num`: Matrix with squared num values. 
#'   - `denom`: Matrix with squared denom values.
#'   - `v`: Same as input `v`.
#'   - `pairwise`: Same as input `pairwise`.
#' @references
#'   Friedman, J. H. and Popescu, B. E. (2008). "Predictive learning via rule
#'     ensembles." The Annals of Applied Statistics. JSTOR, 916–54.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- fx_interaction(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' inter
#' summary(inter)
#' summary(inter, normalize = FALSE, squared = TRUE)
#' 
#' inter <- fx_interaction(
#'   fit, v = names(iris[-1]), X = iris, verbose = FALSE, pairwise = TRUE
#' )
#' summary(inter)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' fx_interaction(fit, v = v, X = iris)
#' fx_interaction(fit, v = v, X = iris, pairwise = TRUE)
#' 
#' # MODEL THREE: Gamma GLM with log link
#' fit <- glm(
#'   Sepal.Length ~ . + Petal.Width:Species, 
#'   data = iris, 
#'   family = Gamma(link = log)
#' )
#' 
#' # No interactions for additive features, at least on link scale
#' fx_interaction(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # On original scale, we have interactions everywhere...
#' fx_interaction(
#'   fit, v = names(iris[-1]), X = iris, verbose = FALSE, type = "response"
#' )
#' 
fx_interaction <- function(object, ...) {
  UseMethod("fx_interaction")
}

#' @describeIn fx_interaction Default method.
#' @export
fx_interaction.default <- function(object, v, X, pred_fun = stats::predict,
                                   pairwise = FALSE, n_max = 200L, w = NULL, 
                                   verbose = TRUE, ...) {
  p <- length(v)
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= c(1L, p),
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
  pd1d <- stats::setNames(vector("list", length = p), v)
  for (z in v) {
    g <- if (is.data.frame(X)) X[[z]] else X[, z]
    pd1d[[z]] <- .center(
      pdp_raw(object = object, v = z, X = X, pred_fun = pred_fun, grid = g, w = w, ...)
    )
    if (verbose) {
      utils::setTxtProgressBar(pb, j)
      j <- j + 1L
    }
  }

  # Preparations
  if (pairwise) {
    combs <- utils::combn(v, 2L, simplify = FALSE)
    m <- length(combs)  # Need to loop over all pairwise combinations
  } else {
    m <- p              # Need to loop only over v
    f <- .center(check_pred(pred_fun(object, X, ...)))
    mean_f_squared <- colMeans(f^2)
  }
  denom <- num <- vector("list", length = m)
  nms <- if (!pairwise) v else sapply(combs, paste, collapse = ":")
  names(denom) <- names(num) <- nms
  
  for (i in seq_len(m)) {
    if (pairwise) {
      z <- combs[[i]]  # The two variables for which we need two-dimensional PDs
      f <- .center(
        pdp_raw(object, v = z, X = X, pred_fun = pred_fun, grid = X[, z], w = w, ...)
      )
      pd_i <- pd1d[[z[1L]]]
      pd_j <- pd1d[[z[2L]]]
    } else {
      z <- v[i]
      not_z <- setdiff(colnames(X), z) 
      pd_j <- .center(
        pdp_raw(
          object, 
          v = not_z, 
          X = X, 
          pred_fun = pred_fun, 
          grid = X[, not_z], 
          w = w,
          compress_grid = FALSE,  # grid has too many columns (saves a very quick check)
          ...
        )
      )
      pd_i <- pd1d[[z]]
    }
    num[[i]] <- colMeans((f - pd_i - pd_j)^2)
    denom[[i]] <- if (pairwise) colMeans(f^2) else mean_f_squared
    
    if (verbose) {
      utils::setTxtProgressBar(pb, j)
      j <- j + 1L
    }
  }
  if (verbose) {
    cat("\n")
  }
  structure(
    list(
      num = do.call(rbind, num), 
      denom = do.call(rbind, denom),
      v = v,
      pairwise = pairwise
    ),
    class = "fx_interaction"
  )
}

#' @describeIn fx_interaction Method for "ranger" models
#' .
#' @export
fx_interaction.ranger <- function(object, v, X,
                                  pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                  pairwise = FALSE, n_max = 200L, 
                                  w = NULL, verbose = TRUE, ...) {
  fx_interaction.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise = pairwise,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' @describeIn fx_interaction Method for "mlr3" models.
#' @export
fx_interaction.Learner <- function(object, v, X,
                                   pred_fun = function(m, X) m$predict_newdata(X)$response,
                                   pairwise = FALSE, n_max = 200L,
                                   w = NULL, verbose = TRUE, ...) {
  fx_interaction.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise = pairwise,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' Interaction Print
#' 
#' Print function for result of [fx_interaction()].
#' 
#' @param x Object to print.
#' @param ... Currently unused.
#' @returns Invisibly, `x` is returned.
#' @export
#' @seealso [fx_interaction()]
print.fx_interaction <- function(x, ...) {
  cat("Interaction statistics. Use summary(...) to extract the results.")
  invisible(x)
}

#' Interaction Summary
#' 
#' Uses the results of [fx_interaction()] to calculate different versions of 
#' Friedman's H, see [fx_interaction()].
#' 
#' @param object Object to summarize.
#' @param normalize Should explained variances be normalized? Default is `TRUE`.
#' @param squared Should squared statistics be returned? Default is `FALSE`. 
#' @param sort Should result be sorted? Default is `TRUE`.
#' @param out_names Optional names of the output columns corresponding to the 
#'   K-dimensional predictions.
#' @param eps Threshold below which num values are set to 0.
#' @param verbose Should message be printed? Default is `TRUE`.
#' @param ... Currently unused.
#' @returns Matrix with statistics (one column per prediction dimension).
#' @export
#' @seealso [fx_interaction()]
summary.fx_interaction <- function(object, normalize = TRUE, squared = FALSE, 
                                   sort = TRUE, out_names = NULL, 
                                   eps = 1e-8, verbose = TRUE, ...) {
  if (verbose) {
    cat(
      if (normalize) "Normalized" else "Unnormalized", 
      "Friedman's H", 
      if (squared) "squared", 
      if (sort) "(sorted)",
      "\n"
    )
  }
  
  num <- .zap_small(object[["num"]], eps = eps)
  num <- fix_names(num, out_names = out_names, prefix = "Stat")
  denom <- fix_names(object[["denom"]], out_names = out_names, prefix = "Stat")
  
  if (normalize) {
    num <- num / denom
  }
  if (!squared) {
    num <- sqrt(num)
  }
  if (!sort) {
    return(num) 
  }
  num[order(-rowSums(num)), , drop = FALSE]
}
