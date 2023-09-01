#' Permutation Importance
#' 
#' Calculates permutation feature importance (PVI) for a set of features or 
#' a set of feature groups `v`.
#' 
#' The PVI of a feature is defined as the increase in the average loss when
#' shuffling the corresponding feature values before calculating predictions.
#' By default, the process is repeated `perms = 4` times, and the results are averaged.
#' 
#' @inheritSection average_loss Losses
#' 
#' @param v Vector of feature names, or named list of feature groups.
#' @param perms Number of permutations (default 4).
#' @param agg_cols Should multivariate losses be summed up? Default is `FALSE`.
#' @param normalize Should importance statistics be divided by average loss?
#'   Default is `FALSE`. If `TRUE`, an importance of 1 means that the average loss
#'   has doubled by shuffling that feature's column.
#' @inheritParams hstats
#' @inheritParams average_loss
#' @returns
#'   An object of class "perm_importance" containing these elements:
#'   - `imp`: (p x d) matrix containing the sorted (average) importance values, i.e.,
#'     a row per feature (group) and a column per loss dimension.
#'   - `SE`: (p x d) matrix with corresponding standard errors of `imp`.
#'      Multiply with `sqrt(perms)` to get standard deviations.
#'   - `perf`: Average loss before shuffling.
#'   - `perms`: Same as input `perms`.
#' @references
#'   Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful:
#'     Variable Importance for Black-Box, Proprietary, or Misspecified Prediction
#'     Models, using Model Class Reliance. Arxiv.
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' v <- setdiff(names(iris), "Sepal.Length")
#' s <- perm_importance(fit, v = v, X = iris, y = iris$Sepal.Length)
#' s
#' s$imp
#' s$SE  # Standard errors
#' plot(s)
#' plot(s, err_type = "sd")  # Standard deviations instead of standard errors
#' 
#' # Groups of features can be passed as named list
#' v <- list(petal = c("Petal.Length", "Petal.Width"), species = "Species")
#' s <- perm_importance(fit, v = v, X = iris, y = iris$Sepal.Length)
#' s
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' s <- perm_importance(fit, v = v, X = iris, y = iris[1:2])
#' s
#' plot(s)
#' plot(s, rotate_x = TRUE, facet_scale = "free_x", err_type = "sd")
perm_importance <- function(object, ...) {
  UseMethod("perm_importance")
}

#' @describeIn perm_importance Default method.
#' @export
perm_importance.default <- function(object, v, X, y, 
                                    pred_fun = stats::predict,
                                    loss = "squared_error", 
                                    perms = 4L, agg_cols = FALSE,
                                    normalize = FALSE, n_max = 10000L,
                                    w = NULL, verbose = FALSE, ...) {
  basic_check(
    X = X, 
    v = unlist(v, use.names = FALSE, recursive = FALSE), 
    pred_fun = pred_fun, 
    w = w
  )
  if (!is.function(loss) && loss == "mlogloss" && NCOL(y) == 1L) {
    y <- stats::model.matrix(~y + 0)
  }
  if (!is.matrix(y)) {
    y <- as.matrix(y)
  }
  stopifnot(
    nrow(y) == nrow(X),
    perms >= 1L
  )
  
  # Reduce size of X, y (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    y <- y[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
  }
  n <- nrow(X)
  p <- length(v)
  if (!is.list(v)) {
    v <- as.list(v)
    names(v) <- v
  }
  
  if (!is.function(loss)) {
    loss <- get_loss_fun(loss)
  }
  
  # Pre-shuffle performance
  L <- loss(y, align_pred(pred_fun(object, X, ...)))
  perf <- wcolMeans(L, w = w)

  # Stack y and X m times
  if (perms > 1L) {
    ind <- rep(seq_len(n), times = perms)
    X <- X[ind, , drop = FALSE]
    y <- y[ind, , drop = FALSE]
  }
  
  shuffle_perf <- function(z, XX) {
    ind <- c(replicate(perms, sample(seq_len(n))))
    if (is.data.frame(XX) && length(z) == 1L) {
      XX[[z]] <- XX[[z]][ind]  # a bit faster
    } else {
      XX[, z] <- XX[ind, z]
    }
    L <- loss(y, align_pred(pred_fun(object, XX, ...)))
    t(wrowmean(L, ngroups = perms, w = w))
  }
  
  # Step 0: Performance after shuffling (expensive)
  if (verbose) {
    pb <- utils::txtProgressBar(1L, max = p, style = 3)
  }
  S <- array(
    dim = c(p, length(perf), perms), dimnames = list(names(v), names(perf), NULL)
  )
  for (j in seq_len(p)) {
    z <- v[[j]]
    S[j, , ] <- shuffle_perf(z, XX = X)
    if (verbose) {
      utils::setTxtProgressBar(pb, j)
    }
  }
  if (verbose) {
    cat("\n")
  }
  
  # Step 1 (optional): Collapse loss dimension
  if (length(perf) > 1L && agg_cols) {
    S <- apply(S, MARGIN = c(1L, 3L), FUN = sum)
    S <- array(
      S, dim = c(nrow(S), 1L, ncol(S)), dimnames = list(rownames(S), NULL, colnames(S))
    )
    perf <- sum(perf)
  }
  
  # Step 2: Collapse over permutations
  SE <- apply(S, MARGIN = 1:2, FUN = stats::sd) / sqrt(perms)
  S <- apply(S, MARGIN = 1:2, FUN = mean)
  
  # Step 3: Subtract perf (Steps 2 and 3 could be swapped)
  S <- sweep(S, MARGIN = 2L, STATS = perf, FUN = "-")
  
  # Step 4 (optional): Normalize
  if (normalize) {
    S <- sweep(S, MARGIN = 2L, STATS = perf, FUN = "/")
    SE <- sweep(SE, MARGIN = 2L, STATS = perf, FUN = "/")
  }
  
  # Step 5: Sort
  ind <- order(-rowSums(S))
  S <- S[ind, , drop = FALSE]
  SE <- SE[ind, , drop = FALSE]
  
  structure(
    list(imp = S, SE = SE, perf = perf, perms = perms), 
    class = "perm_importance"
  )
}

#' @describeIn perm_importance Method for "ranger" models.
#' @export
perm_importance.ranger <- function(object, v, X, y, 
                                   pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                   loss = "squared_error", perms = 4L, 
                                   agg_cols = FALSE, 
                                   normalize = FALSE, n_max = 10000L, 
                                   w = NULL, verbose = FALSE, ...) {
  perm_importance.default(
    object = object,
    v = v,
    X = X,
    y = y,
    pred_fun = pred_fun,
    loss = loss,
    perms = perms,
    agg_cols = agg_cols,
    normalize = normalize,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' @describeIn perm_importance Method for "mlr3" models.
#' @export
perm_importance.Learner <- function(object, v, X, y, 
                                    pred_fun = NULL,
                                    loss = "squared_error", perms = 4L, 
                                    agg_cols = FALSE, 
                                    normalize = FALSE, n_max = 10000L, 
                                    w = NULL, verbose = FALSE, ...) {
  if (is.null(pred_fun)) {
    pred_fun <- mlr3_pred_fun(object, X = X)
  }
  perm_importance.default(
    object = object,
    v = v,
    X = X,
    y = y,
    pred_fun = pred_fun,
    loss = loss,
    perms = perms,
    agg_cols = agg_cols,
    normalize = normalize,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' @describeIn perm_importance Method for DALEX "explainer".
#' @export
perm_importance.explainer <- function(object, 
                                      v = colnames(object[["data"]]), 
                                      X = object[["data"]], 
                                      y = object[["y"]], 
                                      pred_fun = object[["predict_function"]],
                                      loss = "squared_error", 
                                      perms = 4L,
                                      agg_cols = FALSE,
                                      normalize = FALSE,
                                      n_max = 10000L, 
                                      w = object[["weights"]], 
                                      verbose = FALSE, 
                                      ...) {
  perm_importance.default(
    object = object[["model"]],
    v = v,
    X = X,
    y = y,
    pred_fun = pred_fun,
    loss = loss,
    perms = perms,
    agg_cols = agg_cols,
    normalize = normalize,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' Print Method
#' 
#' Print method for object of class "perm_importance".
#'
#' @param x An object of class "perm_importance".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [perm_importance()] for examples.
print.perm_importance <- function(x, ...) {
  print(drop(utils::head(x[["imp"]], n = 15L)))
  invisible(x)
}

#' Plots "perm_importance" Object
#'
#' Plot method for objects of class "perm_importance".
#'
#' @importFrom ggplot2 .data
#' @param x An object of class "perm_importance".
#' @param err_type The error type to show, by default "se" (standard errors). Set to
#'   "sd" for standard deviations (se * sqrt(perms)), or "no" for no bars.
#' @inheritParams plot.hstats
#' @param ... Arguments passed to [ggplot2::geom_bar].
#' @export
#' @returns An object of class "ggplot".
#' @seealso See [perm_importance()] for examples.
plot.perm_importance <- function(x, top_m = 15L, err_type = c("se", "sd", "no"),
                                 facet_scales = "fixed", ncol = 2L, 
                                 rotate_x = FALSE, fill = "#2b51a1", ...) {
  err_type <- match.arg(err_type)
  S <- x[["imp"]]
  err <- x[["SE"]]
  
  if (err_type == "sd") {
    err <- err * sqrt(x[["perms"]])
  }
  S <- utils::head(S, n = top_m)
  err <- utils::head(err, n = top_m)
  df <- transform(mat2df(S), error_ = mat2df(err)[["value_"]])
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = value_, y = variable_)) +
    ggplot2::geom_bar(fill = fill, stat = "identity", ...)
    if (err_type != "no") {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(xmin = value_ - error_, xmax = value_ + error_), 
        width = 0, 
        color = "black"
      )
    }
  if (length(unique(df[["varying_"]])) > 1L) {
    p <- p + ggplot2::facet_wrap("varying_", ncol = ncol, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + rotate_x_labs()
  }
  p + ggplot2::labs(y = ggplot2::element_blank(), x = "Average loss increase")
}
