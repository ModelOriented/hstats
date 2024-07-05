#' Permutation Importance
#' 
#' Calculates permutation importance for a set of features or a set of feature groups. 
#' By default, importance is calculated for all columns in `X` (except column names
#' used as response `y` or as case weight `w`).
#' 
#' The permutation importance of a feature is defined as the increase in the average
#' loss when shuffling the corresponding feature values before calculating predictions.
#' By default, the process is repeated `m_rep = 4` times, and the results are averaged.
#' In most of the cases, importance values should be derived from an independent test
#' data set. Set `normalize = TRUE` to get *relative* increases in average loss.
#' 
#' @inheritSection average_loss Losses
#' 
#' @param v Vector of feature names, or named list of feature groups.
#'   The default (`NULL`) will use all column names of `X` with the following exception: 
#'   If `y` or `w` are passed  as column names, they are dropped.
#' @param m_rep Number of permutations (default 4).
#' @param normalize Should importance statistics be divided by average loss?
#'   Default is `FALSE`. If `TRUE`, an importance of 1 means that the average loss
#'   has been doubled by shuffling that feature's column.
#' @inheritParams hstats
#' @inheritParams average_loss
#' @inherit h2_overall return
#' @references
#'   Fisher A., Rudin C., Dominici F. (2018). All Models are Wrong but many are Useful:
#'     Variable Importance for Black-Box, Proprietary, or Misspecified Prediction
#'     Models, using Model Class Reliance. Arxiv.
#' @export
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' s <- perm_importance(fit, X = iris, y = "Sepal.Length")
#'
#' s
#' s$M
#' s$SE  # Standard errors are available thanks to repeated shuffling
#' plot(s)
#' plot(s, err_type = "SD")  # Standard deviations instead of standard errors
#' 
#' # Groups of features can be passed as named list
#' v <- list(petal = c("Petal.Length", "Petal.Width"), species = "Species")
#' s <- perm_importance(fit, X = iris, y = "Sepal.Length", v = v, verbose = FALSE)
#' s
#' plot(s)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[, 1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' s <- perm_importance(fit, X = iris[, 3:5], y = iris[, 1:2], normalize = TRUE)
#' s
#' plot(s)
#' plot(s, swap_dim = TRUE, top_m = 2)
perm_importance <- function(object, ...) {
  UseMethod("perm_importance")
}

#' @describeIn perm_importance Default method.
#' @export
perm_importance.default <- function(object, X, y, v = NULL,
                                    pred_fun = stats::predict,
                                    loss = "squared_error", 
                                    m_rep = 4L, agg_cols = FALSE,
                                    normalize = FALSE, n_max = 10000L,
                                    w = NULL, verbose = TRUE, ...) {
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    is.function(pred_fun),
    m_rep >= 1L
  )
  
  # Are y column names or a vector/matrix?
  y2 <- prepare_y(y = y, X = X)
  y <- y2[["y"]]
  y_names <- y2[["y_names"]]
  
  # Is w a column name or a vector?
  if (!is.null(w)) {
    w2 <- prepare_w(w = w, X = X)
    w <- w2[["w"]]
    w_name <- w2[["w_name"]]
  }
  
  # Prepare v
  if (is.null(v)) {
    v <- colnames(X)
    if (!is.null(w) && !is.null(w_name)) {
      v <- setdiff(v, w_name)
    }
    if (!is.null(y_names)) {
      v <- setdiff(v, y_names)
    }
  } else {
    v_c <- unlist(v, use.names = FALSE, recursive = FALSE)
    stopifnot(all(v_c %in% colnames(X)))
  }
  if (!is.list(v)) {
    v <- as.list(v)
    names(v) <- v
  }
  p <- length(v)
 
  # Reduce size of X, y (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (is.vector(y)) {
      y <- y[ix]
    } else {
      y <- y[ix, , drop = FALSE]
    }
    if (!is.null(w)) {
      w <- w[ix]
    }
  }
  n <- nrow(X)
  
  if (!is.function(loss)) {
    loss <- get_loss_fun(loss)
  }
  
  # Pre-shuffle performance
  pred <- prepare_pred(pred_fun(object, X, ...))
  perf <- wcolMeans(loss(y, pred), w = w)

  # Stack y and X m times
  if (m_rep > 1L) {
    ind <- rep.int(seq_len(n), m_rep)
    X <- rep_rows(X, ind)
    if (is.vector(y)) {
      y <- y[ind]
    } else {
      y <- y[ind, , drop = FALSE]
    }
  }
  
  shuffle_perf <- function(z, XX) {
    # Shuffle within n rows (could be slightly sped-up via lapply())
    ind <- c(replicate(m_rep, sample.int(n)))
    
    if (is.matrix(XX) || length(z) > 1L) {
      XX[, z] <- XX[ind, z]
    } else {
      XX[[z]] <- XX[[z]][ind]
    }
    pred <- prepare_pred(pred_fun(object, XX, ...))
    t(wrowmean(loss(y, pred), ngroups = m_rep, w = w))
  }
  
  # Step 0: Performance after shuffling (expensive)
  if (verbose) {
    pb <- utils::txtProgressBar(max = p, style = 3)
  }
  S <- array(
    dim = c(p, length(perf), m_rep), dimnames = list(names(v), names(perf), NULL)
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
  SE <- apply(S, MARGIN = 1:2, FUN = stats::sd) / sqrt(m_rep)
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
    list(
      M = S, 
      SE = SE, 
      m_rep = m_rep,
      statistic = "perm_importance",
      description = paste0("Permutation importance", if (normalize) " (relative)")
    ), 
    class = "hstats_matrix"
  )
}

#' @describeIn perm_importance Method for "ranger" models.
#' @export
perm_importance.ranger <- function(object, X, y, v = NULL,
                                   pred_fun = function(m, X, ...)
                                     stats::predict(m, X, ...)$predictions,
                                   loss = "squared_error", m_rep = 4L, 
                                   agg_cols = FALSE, 
                                   normalize = FALSE, n_max = 10000L, 
                                   w = NULL, verbose = TRUE, ...) {
  perm_importance.default(
    object = object,
    X = X,
    y = y,
    v = v,
    pred_fun = pred_fun,
    loss = loss,
    m_rep = m_rep,
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
                                      X = object[["data"]], 
                                      y = object[["y"]],
                                      v = NULL,
                                      pred_fun = object[["predict_function"]],
                                      loss = "squared_error", 
                                      m_rep = 4L,
                                      agg_cols = FALSE,
                                      normalize = FALSE,
                                      n_max = 10000L, 
                                      w = object[["weights"]], 
                                      verbose = TRUE, 
                                      ...) {
  perm_importance.default(
    object = object[["model"]],
    X = X,
    y = y,
    v = v,
    pred_fun = pred_fun,
    loss = loss,
    m_rep = m_rep,
    agg_cols = agg_cols,
    normalize = normalize,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}
