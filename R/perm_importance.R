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
#'   - `imp_raw`: (p x d x m) array containing the importance values.
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
#' s <- perm_importance(fit, v = v, X = iris, y = iris$Sepal.Length, m_rep = 4)
#' s
#' summary(s, normalize = TRUE)
#' plot(s, top_m = 3)
#'
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' perm_importance(fit, v = v, X = iris, y = iris[1:2])
#' s <- perm_importance(fit, v = v, X = iris, y = iris[1:2], m_rep = 4)
#' plot(s, agg_cols = "no", normalize = TRUE)
perm_importance <- function(object, ...) {
  UseMethod("perm_importance")
}

#' @describeIn perm_importance Default method.
#' @export
perm_importance.default <- function(object, v, X, y, 
                                    pred_fun = stats::predict,
                                    loss = "squared_error", 
                                    m_rep = 1L, n_max = 10000L, 
                                    w = NULL, verbose = TRUE, ...) {
  basic_check(X = X, v = v, pred_fun = pred_fun)
  if (!is.matrix(y)) {
    y <- as.matrix(y)
  }
  stopifnot(
    nrow(y) == nrow(X),
    m_rep >= 1L
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
  if (verbose) {
    pb <- utils::txtProgressBar(1L, max = p, style = 3)
  }
  imp_raw <- array(
    dim = c(p, length(perf), m_rep), dimnames = list(v, names(perf), NULL)
  )
  for (j in seq_len(p)) {
    z <- v[j]
    imp_raw[z, , ] <- shuffle_perf(z, XX = X)
    if (verbose) {
      utils::setTxtProgressBar(pb, j)
    }
  }
  if (verbose) {
    cat("\n")
  }
  
  # Subtract original performance
  imp_raw <- sweep(imp_raw, MARGIN = 2:3, STATS = perf, FUN = "-")
  
  # Organize output
  out <- list(
    imp = apply(imp_raw, FUN = mean, MARGIN = 1:2),
    perf = perf,
    imp_raw = imp_raw,
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
                                   m_rep = 1L, n_max = 10000L, 
                                   w = NULL, verbose = TRUE, ...) {
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
    verbose = verbose,
    ...
  )
}

#' @describeIn perm_importance Method for "mlr3" models.
#' @export
perm_importance.Learner <- function(object, v, X, y, 
                                    pred_fun = NULL,
                                    loss = "squared_error", 
                                    m_rep = 1L, n_max = 10000L, 
                                    w = NULL, verbose = TRUE, ...) {
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
                                      m_rep = 1L, 
                                      n_max = 10000L, 
                                      w = object[["weights"]], 
                                      verbose = TRUE, 
                                      ...) {
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
    verbose = verbose,
    ...
  )
}

#' Print Method
#' 
#' Print method for object of class "perm_importance". 
#' Shows results of top 6 predictors averaged over `m_rep`.
#'
#' @param x An object of class "perm_importance".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [perm_importance()] for examples.
print.perm_importance <- function(x, ...) {
  cat("Highest permutation importances:\n\n")
  print(drop(prep_imp(x, top_m = 6L)[["imp"]]))
  invisible(x)
}

#' Summary Method
#' 
#' Summary method for "perm_importance" object.
#' 
#' @param object An object of class "perm_importance".
#' @inheritParams prep_imp
#' @param ... Currently not used.
#' @inherit prep_imp return
#' @export
#' @seealso See [perm_importance()] for examples.
summary.perm_importance <- function(object, normalize = FALSE, sort = TRUE, top_m = 15L, 
                                    agg_cols = c("sum", "mean", "max", "no"),
                                    err_type = c("se", "sd", "no"), ...) {
  agg_cols <- match.arg(agg_cols)
  err_type <- match.arg(err_type)
  out <- prep_imp(
    x = object,
    normalize = normalize,
    sort = sort,
    top_m = top_m,
    agg_cols = agg_cols,
    err_type = err_type
  )

  cat("Permutation importances:\n\n")
  print(drop(out[["imp"]]))
  cat("\n")
  invisible(out)
}

#' Processing of Importance
#' 
#' Applies the following steps:   
#' 1. Aggregate columns
#' 2. Divide by perf
#' 3. Calculate sd/se
#' 4. Average rows
#' 5. Sort
#' 6. Top_m
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Object of class "perm_importance".
#' @param normalize Should importance statistics be divided by performance?
#'   Default is `FALSE`.
#' @param sort Should results be sorted by importance? Default is `TRUE`.
#'   The multivariate case is sorted by row sums.
#' @param top_m Of how many features should importance values be shown?
#'   Default is `Inf`, i.e., show all.
#' @param agg_cols Name of function used to collapse multivariate losses.
#'   Default is `"sum"`. Set to `"no"` for no aggregation.
#' @param err_type Should standard errors ("se", default) or standard deviations ("sd") 
#'   be calculated? Only relevant if `m_rep > 1`. Set to "no" to get `NA` instead.
#' @returns 
#'   A list with two matrices: `imp` contains the importance values per feature,
#'   while `err` represents the uncertainty of these values.
prep_imp <- function(x, normalize = FALSE, sort = TRUE, top_m = Inf, 
                     agg_cols = c("sum", "mean", "max", "no"),
                     err_type = c("se", "sd", "no")) {
  agg_cols <- match.arg(agg_cols)
  err_type <- match.arg(err_type)

  S <- x[["imp_raw"]]
  perf <- x[["perf"]]
  K <- ncol(S)
  
  if (K > 1L && agg_cols != "no") {
    f <- match.fun(agg_cols)
    S <- apply(S, MARGIN = c(1L, 3L), FUN = f)
    S <- array(
      S, dim = c(nrow(S), 1L, ncol(S)), dimnames = list(rownames(S), NULL, colnames(S))
    )
    perf <- f(perf)
  }
  if (normalize) {
    S <- sweep(S, MARGIN = 2L, STATS = perf, FUN = "/")
  }
  
  # Aggregate over m_rep
  err <- apply(S, MARGIN = 1:2, FUN = sd)
  if (err_type == "se") {
    err <- err / sqrt(x[["m_rep"]])
  } else if (err_type == "no") {
    err[] <- NA
  }
  S <- apply(S, MARGIN = 1:2, FUN = mean)
  
  if (sort) {
    ind <- order(-rowSums(S))
    S <- S[ind, , drop = FALSE]
    err <- err[ind, , drop = FALSE]
  }
  
  S <- utils::head(S, n = top_m)
  err <- utils::head(err, n = top_m)
  list(imp = S, err = err)
}

#' Plots "perm_importance" Object
#'
#' Plot method for objects of class "perm_importance".
#'
#' @importFrom ggplot2 .data
#' @inheritParams H2_overall
#' @inheritParams summary.perm_importance
#' @param ... Arguments passed to [ggplot2::geom_bar].
#' @export
#' @returns An object of class "ggplot".
#' @seealso See [perm_importance()] for examples.
plot.perm_importance <- function(x, normalize = FALSE, sort = TRUE, top_m = 15L, 
                                 agg_cols = c("sum", "mean", "max", "no"),
                                 err_type = c("se", "sd", "no"),
                                 fill = "#2b51a1", ...) {
  agg_cols <- match.arg(agg_cols)
  err_type <- match.arg(err_type)
  res <- prep_imp(
    x = x,
    normalize = normalize,
    sort = sort,
    top_m = top_m,
    agg_cols = agg_cols,
    err_type = err_type
  )
  
  df <- transform(mat2df(res[["imp"]]), error_ = mat2df(res[["err"]])[["value_"]])

  p <- ggplot2::ggplot(
    df, 
    ggplot2::aes(
      x = value_, y = variable_, xmin = value_ - error_, xmax = value_ + error_
    )
  ) +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab("Value")

  if (length(unique(df[["varying_"]])) == 1L) {
    p <- p + ggplot2::geom_bar(fill = fill, stat = "identity", ...)
    if (err_type != "no") {
      p <- p + ggplot2::geom_errorbar(width = 0, color = "black")
    }
  } else {
    p <- p +
      ggplot2::geom_bar(
        ggplot2::aes(fill = varying_), stat = "identity", position = "dodge", ...
      ) +
      ggplot2::labs(fill = "Dim")
    if (err_type != "no") {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(group = varying_),
        width = 0, 
        color = "black", 
        position = ggplot2::position_dodge(0.9)
      )
    }
  }
  p
}
