#' Permutation Importance
#'
#' Calculates permutation importance for a set `v` of features. 
#' It shows the absolute (or relative) increase in the average loss when 
#' shuffling the corresponding feature column. Note that the model is never refitted.
#'
#' @inheritParams hstats
#' @param y Numeric vector or matrix of observed values of the response. In case of a 
#'   classification situation with m categories (`loss = "mlogloss"`),
#'   this is a matrix with m one-hot-encoded columns, e.g., created by 
#'   `model.matrix(y ~ 0, data = data)`.
#' @param loss One of "squared_error", "logloss", "mlogloss", "poisson",
#'   "gamma", "absolute_error", or a loss function that turns observed and predicted 
#'   values (vectors or matrices) into a vector or matrix of unit losses.
#' @param m_rep Number of permutations (default 1).
#' @returns
#'   An object of class "perm_importance" containing these elements:
#'   - `imp`: (p x d) matrix with importance values (averaged over repetitions).
#'     (One row per variable, one column per loss dimension).
#'   - `perf`: Average loss before shuffling.
#'   - `imp_m`: (p x d x m) array containing the importance values.
#'     (One row per variable, one column per loss dimension, one slice per repetition).
#'   - `v`: Same as input `v`.
#'   - `m_rep`: Same as input `m_rep`.
#' @references
#'   Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful:
#'     Variable Importance for Black-Box, Proprietary, or Misspecified Prediction
#'     Models, using Model Class Reliance. Arxiv.
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' v <- setdiff(names(iris), "Sepal.Length")
#' perm_importance(fit, v = v, X = iris, y = iris$Sepal.Length)
#' perm_importance(fit, v = v, X = iris, y = iris$Sepal.Length, m_rep = 4)
#'
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' perm_importance(fit, v = v, X = iris, y = iris[1:2])
#' perm_importance(fit, v = v, X = iris, y = iris[1:2], m_rep = 4)
perm_importance <- function(object, ...) {
  UseMethod("perm_importance")
}

#' @describeIn perm_importance Default method.
#' @export
perm_importance.default <- function(object, v, X, y, 
                                    pred_fun = stats::predict,
                                    loss = "squared_error", 
                                    m_rep = 1L, n_max = 10000L, 
                                    w = NULL, ...) {
  basic_check(X = X, v = v, pred_fun = pred_fun)
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
  
  # Pre-shuffle performance
  L <- loss(y, align_pred(pred_fun(object, X, ...)))
  perf <- wcolMeans(L, w = w)

  # Stack y and X m times
  if (m_rep > 1L) {
    ind <- rep(seq_len(n), times = m_rep)
    X <- X[ind, , drop = FALSE]
    y <- y[ind, , drop = FALSE]
  }
  
  #  Performance after shuffling (m rows)
  shuffle_perf <- function(z, XX) {
    ind <- c(replicate(m_rep, sample(seq_len(n))))
    XX[, z] <- XX[ind, z]
    L <- loss(y, align_pred(pred_fun(object, XX, ...)))
    t(wrowmean(L, ngroups = m_rep, w = w))
  }

  # Loop over v
  imp_m <- array(
    dim = c(length(v), length(perf), m_rep), dimnames = list(v, names(perf), NULL)
  )
  for (z in v) {
    imp_m[z, , ] <- shuffle_perf(z, XX = X)
  }
  
  # Subtract original performance
  imp_m <- sweep(imp_m, MARGIN = 2:3, STATS = perf, FUN = "-")
  
  # Organize output
  out <- list(
    imp = apply(imp_m, FUN = mean, MARGIN = 1:2),
    perf = perf,
    imp_m = imp_m,
    v = v, 
    m_rep = m_rep
  )
  class(out) <- "perm_importance"
  out
}

#' @describeIn perm_importance Method for "ranger" models.
#' @export
perm_importance.ranger <- function(object, v, X, y, 
                                   pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                   loss = "squared_error", 
                                   m_rep = 1L, n_max = 10000L, w = NULL, ...) {
  perm_importance.default(
    object = object,
    v = v,
    X = X,
    y = y,
    pred_fun = pred_fun,
    loss = loss,
    m_rep = m_rep,
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
                                    m_rep = 1L, n_max = 10000L, w = NULL, ...) {
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
    m_rep = m_rep,
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
                                      m_rep = 1L, n_max = 10000L, 
                                      w = object[["weights"]], ...) {
  perm_importance.default(
    object = object[["model"]],
    v = v,
    X = X,
    y = y,
    pred_fun = pred_fun,
    loss = loss,
    m_rep = m_rep,
    n_max = n_max,
    w = w,
    ...
  )
}

#' Print Method
#' 
#' Print method for object of class "perm_importance". 
#' Shows results of top 5 predictors averaged over `m_rep`.
#'
#' @param x An object of class "perm_importance".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [perm_importance()] for examples.
print.perm_importance <- function(x, ...) {
  print(x[["imp"]])
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
# summary.perm_importance <- function(object, normalize = FALSE, 
#                                     agg_fun = sum,
#                                     sort = TRUE, top_m = 15L, ...) {
#   out <- postprocess(
#     num = object[["importance"]],
#     denom = object[["perf"]],
#     normalize = normalize, 
#     squared = TRUE, 
#     sort = sort, 
#     top_m = top_m, 
#     eps = 0
#   )
#   
#   cat("Highest permutation importance:\n")
#   print(out)
#   cat("\n")
#   invisible(out)
# }

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
