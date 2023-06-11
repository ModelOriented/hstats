#' Calculations behind Interaction Statistics
#' 
#' Expensive crunching behind interaction statistics such as:
#' - Friedman and Popescu's \eqn{H^2_j} of overall interaction strength per feature.
#' - Friedman and Popescu's \eqn{H^2_{jk}} of pairwise interaction strengths.
#' - Unnormalized pairwise statistic \eqn{H_{jk}}.
#' - ...
#'  
#' @inheritParams pd_raw
#' @param pairwise_limit Pairwise evaluation of \eqn{p} features require \eqn{p(p-1)/2}
#'   expensive calculations. Therefore, we limit them to features with minimal relative
#'   overall interaction strength (Friedman and Popescu's \eqn{H^2_j}) of more than this
#'   value. Set to 0 to not miss any pairwise interaction. Set to 1 to not calculate
#'   any pairwise interactions.
#' @param verbose Should a progress bar be shown? The default is `TRUE`.
#' @returns 
#'   An object of class "i_compute", containing these elements:
#'   - `f`: Matrix with predictions.
#'   - `F_j`: List of matrices, each representing univariable PDs.
#'   - `F_not_j`: List of matrices, each representing the PDs of all variables != j.
#'   - `F_jk`: List of matrices, each representing bivariate PDs.
#'   - `w`: Same as input w.
#'   - `H2_j`: Matrix of Friedman and Popescu's \eqn{H^2_j} (one row per variable pair).
#'   - `v`: Same as input `v`.
#'   - `v_pairwise`: Subset of `v` with `H2_j` larger than `pairwise_limit`. Pairwise
#'     PDs are calculated only for this subset.
#'   - `pairwise_limit`: Same as input `pairwise_limit`.
#'   - `combs`: List of variable pairs for which pairwise PDs are available.
#' @references
#'   Friedman, Jerome H., and Bogdan E. Popescu. "Predictive Learning via Rule Ensembles."
#'     The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- i_compute(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' inter
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- i_compute(fit, v = v, X = iris, verbose = FALSE)
#' inter

#' # MODEL THREE: Gamma GLM with log link
#' fit <- glm(Sepal.Length ~ ., data = iris, family = Gamma(link = log))
#' 
#' # No interactions for additive features, at least on link scale
#' inter <- i_compute(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' inter
#' 
#' # On original scale, we have interactions everywhere...
#' inter <- i_compute(
#'   fit, v = names(iris[-1]), X = iris, type = "response", verbose = FALSE
#' )
#' inter
#' 
i_compute <- function(object, ...) {
  UseMethod("i_compute")
}

#' @describeIn i_compute Default method.
#' @export
i_compute.default <- function(object, v, X, pred_fun = stats::predict,
                              pairwise_limit = 0.001, n_max = 200L, w = NULL, 
                              verbose = TRUE, ...) {
  .basic_check(X = X, v = v, pred_fun = pred_fun, w = w)
  
  # Reduce size of X (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
  }
  
  # Predictions ("F" in Friedman and Popescu) always calculated (cheap)
  f <- .center(check_pred(pred_fun(object, X, ...)), w = w)
  
  # Initialize first progress bar
  p <- length(v)
  show_bar <- verbose && p >= 2L
  if (show_bar) {
    cat("Univariate calculations...\n")
    pb <- utils::txtProgressBar(1L, max = p, style = 3)
  }
  
  F_j <- F_not_j <- stats::setNames(vector("list", length = p), v)
  for (j in seq_len(p)) {
    # Main effect of x_j
    z <- v[j]
    F_j[[z]] <- .center(
      pd_raw(
        object = object, 
        v = z, 
        X = X, 
        grid = if (is.data.frame(X)) X[[z]] else X[, z],
        pred_fun = pred_fun, 
        n_max = n_max, # No effect
        w = w,
        check = FALSE, # Already done
        ...
      ),
      w = w
    )
    
    # Total effect of all other features
    not_z <- setdiff(colnames(X), z)
    F_not_j[[z]] <- .center(
      pd_raw(
        object, 
        v = not_z, 
        X = X, 
        grid = X[, not_z],
        pred_fun = pred_fun,
        n_max = n_max, # No effect
        w = w,
        compress_grid = FALSE,  # grid has too many columns (saves a very quick check)
        check = FALSE, # Already done
        ...
      ),
      w = w
    )
    
    if (show_bar) {
      utils::setTxtProgressBar(pb, j)
    }
  }
  
  if (show_bar) {
    cat("\n")
  }
  
  # Pairwise stats are calculated only for variables with minimal overall interactions
  # For multivariate predictions, we take their maximum in order to not miss anything
  H2_j <- get_H2_j(v = v, f = f, F_j = F_j, F_not_j = F_not_j, w = w)
  rowwise_max <- apply(H2_j, MARGIN = 1L, FUN = max)
  v_pairwise <- names(which(rowwise_max > pairwise_limit))

  if (length(v_pairwise) >= 2L) {
    combs <- utils::combn(v_pairwise, 2L, simplify = FALSE)
    n_combs <- length(combs)
    F_jk <- vector("list", length = n_combs)
    names(F_jk) <- names(combs) <- sapply(combs, paste, collapse = ":")
    
    show_bar <- verbose && n_combs >= 2L
    if (show_bar) {
      cat("Bivariate calculations...\n")
      pb <- utils::txtProgressBar(1L, max = n_combs, style = 3)
    }
  
    for (i in seq_len(n_combs)) {
      z <- combs[[i]]
      F_jk[[i]] <- .center(
        pd_raw(
          object, 
          v = z, 
          X = X, 
          grid = X[, z],
          pred_fun = pred_fun,
          n_max = n_max, # No effect
          w = w,
          check = FALSE, # Already done
          ...
        ),
        w = w
      )
      if (show_bar) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (show_bar) {
      cat("\n")
    }
  } else {
    F_jk <- combs <- list()
  }
  structure(
    list(
      f = f, 
      F_j = F_j, 
      F_not_j = F_not_j, 
      F_jk = F_jk,
      w = w,
      H2_j = H2_j,
      v = v,
      v_pairwise = v_pairwise,
      pairwise_limit = pairwise_limit,
      combs = combs
    ), 
    class = "interactions"
  )
}

#' @describeIn i_compute Method for "ranger" models
#' .
#' @export
i_compute.ranger <- function(object, v, X,
                             pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                             pairwise_limit = 0.001, n_max = 200L, 
                             w = NULL, verbose = TRUE, ...) {
  i_compute.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_limit = pairwise_limit,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' @describeIn i_compute Method for "mlr3" models.
#' @export
i_compute.Learner <- function(object, v, X,
                              pred_fun = function(m, X) m$predict_newdata(X)$response,
                              pairwise_limit = 0.001, n_max = 200L,
                              w = NULL, verbose = TRUE, ...) {
  i_compute.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_limit = pairwise_limit,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}
