#' Permutation Importance
#'
#' Calculates permutation importance for a set of features. 
#' It shows the absolute (or relative) increase in the average loss when 
#' shuffling the corresponding feature column. Note that the model is never refitted 
#' in this process.
#'
#' @inheritParams hstats
#' @param y Numeric vector or matrix of observed values of the response. In case of a 
#'   classification situation with m categories (`loss = "mlogloss"`),
#'   this is a matrix with m one-hot-encoded columns, e.g., created by 
#'   `model.matrix(y ~ 0, data = data)`.
#' @param loss One of "squared_error", "logloss", "mlogloss", "poisson",
#'   "gamma", "absolute_error", or a loss function that turns observed and predicted 
#'   values (vectors or matrices) into a vector/matrix of unit losses.
#' @param reduce_loss How should multivariate losses be aggregated to a single number
#'   *per row*? One of "sum" (default), "mean", or "no" (no aggregation).
#' @param m_repetitions Number of permutations. Defaults to 1. If larger than one,
#'   standard deviations and standard errors can be plotted.
#' @returns
#'   An object of class "perm_importance" containing these elements:
#'   - `importance`: Matrix containing the importance values
#'     (one row per variable, one column per loss dimension).
#'   - `std`: Matrix with corresponding sample standard deviations across repetitions 
#'     (`NA` if `m_repetitions = 1`).
#'   - `perf`: Average loss before shuffling.
#'   - `v`: Same as input `v`.
#'   - `m_repetitions`: Same as input `m_repetitions`.
#' @references
#'   Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful:
#'     Variable Importance for Black-Box, Proprietary, or Misspecified Prediction
#'     Models, using Model Class Reliance. Arxiv.
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' drop1(fit)
#' s <- perm_importance(fit, v = names(iris[-1]), X = iris, y = iris$Sepal.Length)
#' s
#'
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' summary(fit)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' s <- perm_importance(fit, v = v, X = iris, y = iris[1:2])
#' s
#' 
#' s2 <- perm_importance(fit, v = v, X = iris, y = iris[1:2], reduce_loss = "no")
#' s2
perm_importance <- function(object, ...) {
  UseMethod("perm_importance")
}

#' @describeIn perm_importance Default method.
#' @export
perm_importance.default <- function(object, v, X, y, pred_fun = stats::predict,
                                    loss = "squared_error", 
                                    reduce_loss = c("sum", "mean", "no"),
                                    m_repetitions = 1L, n_max = 10000L, w = NULL, ...) {
  basic_check(X = X, v = v, pred_fun = pred_fun)
  reduce_loss <- match.arg(reduce_loss)
  if (!is.matrix(y)) {
    y <- as.matrix(y)
  }
  stopifnot(nrow(y) == nrow(X))
  
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
  
  if (!is.function(loss)) {
    loss <- switch(
      loss,
      squared_error = loss_squared_error,
      logloss = loss_logloss,
      mlogloss = loss_mlogloss,
      poisson = loss_poisson,
      gamma = loss_gamma,
      absolute_error = loss_absolute_error,
      stop("Unknown loss function.")
    )
  }
  
  loss2 <- function(X) {
    L <- loss(y, align_pred(pred_fun(object, X, ...)))
    if (NCOL(L) >= 2L && reduce_loss != "no") {
      L <- if (reduce_loss == "sum") rowSums(L) else rowMeans(L)
    }
    if (!is.matrix(L)) as.matrix(L) else L
  }
  
  # Original performance (a vector)
  perf0 <- wcolMeans(loss2(X), w = w)
  
  # Stack y and X m times
  if (m_repetitions > 1L) {
    ind <- rep(seq_len(n), times = m_repetitions)
    X <- X[ind, , drop = FALSE]
    y <- y[ind, , drop = FALSE]
  }
  
  # Average loss after shuffling m times
  shuffle_perf <- function(z, X) {
    ind <- c(replicate(m_repetitions, sample(seq_len(n))))
    X[, z] <- X[ind, z]
    perf_per_m <- wrowmean(loss2(X), ngroups = m_repetitions, w = w)
    list(perf = colMeans(perf_per_m), std = apply(perf_per_m, 2L, FUN = stats::sd))
  }

  # Loop over v
  imp <- std <- matrix(
    nrow = length(v), ncol = length(perf0), dimnames = list(v, names(perf0))
  )
  for (z in v) {
    temp <- shuffle_perf(z, X = X)
    imp[z, ] <- temp[["perf"]] - perf0
    std[z, ] <- temp[["std"]]
  }
  
  # Organize output
  out <- list(
    importance = imp,
    std = std,
    perf = perf0,
    v = v,
    m_repetitions = m_repetitions
  )
  return(structure(out, class = "perm_importance"))
}

#' @describeIn perm_importance Method for "ranger" models.
#' @export
perm_importance.ranger <- function(object, v, X, y, 
                                   pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                   loss = "squared_error", 
                                   reduce_loss = c("sum", "mean", "no"),
                                   m_repetitions = 1L, n_max = 10000L, w = NULL, ...) {
  perm_importance.default(
    object = object,
    v = v,
    X = X,
    y = y,
    pred_fun = pred_fun,
    loss = loss,
    reduce_loss = reduce_loss,
    m_repetitions = m_repetitions,
    n_max = n_max,
    w = w,
    ...
  )
}

#' @describeIn perm_importance Method for "mlr3" models.
#' @export
perm_importance.Learner <- function(object, v, X, y, 
                                    pred_fun = NULL,
                                    loss = "squared_error", 
                                    reduce_loss = c("sum", "mean", "no"),
                                    m_repetitions = 1L, n_max = 10000L, w = NULL, ...) {
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
    reduce_loss = reduce_loss,
    m_repetitions = m_repetitions,
    n_max = n_max,
    w = w,
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
                                      reduce_loss = c("sum", "mean", "no"),
                                      m_repetitions = 1L, n_max = 10000L, 
                                      w = object[["weights"]], ...) {
  perm_importance.default(
    object = object[["model"]],
    v = v,
    X = X,
    y = y,
    pred_fun = pred_fun,
    loss = loss,
    reduce_loss = reduce_loss,
    m_repetitions = m_repetitions,
    n_max = n_max,
    w = w,
    ...
  )
}

#' Print Method
#' 
#' Print method for object of class "perm_importance". Shows results of top 5 predictors.
#'
#' @param x An object of class "perm_importance".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [perm_importance()] for examples.
print.perm_importance <- function(x, ...) {
  summary(x, top_m = 5L)
  invisible(x)
}

#' Summary Method
#' 
#' Summary method for "perm_importance" object.
#' 
#' @param object An object of class "perm_importance".
#' @param normalize Should mean loss improvements be divided by the original 
#'   mean losses? Default is `FALSE`. If `TRUE`, a value of 2 means that
#'   the mean loss doubled by shuffling.
#' @inheritParams H2_overall
#' @param ... Currently not used.
#' @returns Matrix of variable importance values.
#' @export
#' @seealso See [perm_importance()] for examples.
summary.perm_importance <- function(object, normalize = FALSE, 
                                    sort = TRUE, top_m = 15L, ...) {
  out <- postprocess(
    num = object[["importance"]],
    denom = object[["perf"]],
    normalize = normalize, 
    squared = TRUE, 
    sort = sort, 
    top_m = top_m, 
    eps = 0
  )
  
  cat("Highest permutation importance:\n")
  print(out)
  cat("\n")
  invisible(out)
}

#' #' Plots "perm_importance" Object
#' #' 
#' #' Plot method for objects of class "perm_importance". Can do (grouped) line plots or 
#' #' heatmaps.
#' #' 
#' #' @importFrom ggplot2 .data
#' #' @inheritParams H2_overall
#' #' @inheritParams summary.perm_importance
#' #' @param ... Arguments passed to geometries.
#' #' @export
#' #' @returns An object of class "ggplot".
#' #' @seealso See [perm_importance()] for examples.
#' plot.perm_importance <- function(x, normalize = FALSE, 
#'                                  sort = TRUE, top_m = 15L,
#'                                  rotate_x = FALSE, fill = "#2b51a1", ...) {
#'   
#'   
#'   p <- ggplot2::ggplot(mat2df(x), ggplot2::aes(x = value_, y = variable_)) +
#'     ggplot2::ylab(ggplot2::element_blank()) +
#'     ggplot2::xlab("Value")
#'   
#'   if (ncol(x) == 1L) {
#'     p + ggplot2::geom_bar(fill = fill, stat = "identity", ...)
#'   } else {
#'     p + 
#'       ggplot2::geom_bar(
#'         ggplot2::aes(fill = varying_), stat = "identity", position = "dodge", ...
#'       ) + 
#'       ggplot2::labs(fill = "Response")
#'   }
#' }
