#' PD Importance
#' 
#' The sample variance of the partial dependence function \eqn{\textrm{PD}_{j}} of 
#' feature \eqn{X^{(j)}} evaluated over a data set \eqn{D} can be used as measure 
#' of **main effect importance**, defined as
#' \eqn{\textrm{PDI}(j, D) = \textrm{Var}\left(\sum_{i \in D} \textrm{PD}_{j}(x_i^{(j)})\right)}.
#' Similarly, we can define the PD importance of two or more features together,
#' measuring their joint effect (main effect plus interaction).
#'  
#' @inheritParams fx_pdp
#' @param v Vector or list of feature names for which PD importance is to be calculated. 
#'   If passed as list, *vectors* of feature names are evaluted together.
#' @param sort Should the result be sortedby importance? Default is `TRUE`.
#' @param verbose Should a progress bar be shown? Default is `TRUE`.
#' @returns 
#'   A `data.frame` with one column holding the feature name(s), and K columns with the 
#'   PD importance for the K components of the predict function.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' fx_importance(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' v <- list("Sepal.Width", c("Petal.Width", "Petal.Length"), "Species")
#' fx_importance(fit, v = v, X = iris, verbose = FALSE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' fx_importance(fit, v = v, X = iris)
#' 
#' # MODEL THREE: Gamma GLM with log link
#' fit <- glm(Sepal.Length ~ ., data = iris, family = Gamma(link = log))
#' 
#' fx_importance(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # On original scale
#' fx_importance(fit, v = names(iris[-1]), X = iris, verbose = FALSE, type = "response")
fx_importance <- function(object, ...) {
  UseMethod("fx_importance")
}

#' @describeIn fx_importance Default method.
#' @export
fx_importance.default <- function(object, v, X, pred_fun = stats::predict,
                                  n_max = 300L, out_names = NULL, 
                                  w = NULL, sort = TRUE, verbose = TRUE, ...) {
  p <- length(v)
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= 2:1,
    all(unique(unlist(v, use.names = FALSE)) %in% colnames(X)),
    is.function(pred_fun),
    is.null(w) || length(w) == nrow(X),
    p >= 1L
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
    pb <- utils::txtProgressBar(1L, p, style = 3)
  }
  
  # Univariate PDs (required for both pairwise TRUE/FALSE)
  imp <- vector("list", length(v))
  for (i in seq_along(v)) {
    z <- v[[i]]
    g <- if (is.data.frame(X) && length(z) == 1L) X[[z]] else X[, z]
    pd <- pdp_raw(
      object = object, v = z, X = X, pred_fun = pred_fun, grid = g, w = w, ...
    )
    imp[[i]] <- apply(pd, 2L, FUN = stats::var)
    if (verbose) {
      utils::setTxtProgressBar(pb, i)
    }
  } 
  if (verbose) {
    cat("\n")
  }
  
  imp <- do.call(rbind, imp)
  imp <- fix_names(imp, out_names = out_names, prefix = "Imp")
  V <- sapply(v, paste, collapse = ":", USE.NAMES = FALSE)
  out <- data.frame(V, imp)
  if (sort) {
    out <- out[order(-rowSums(imp)), ]
  }
  out
}


#' @describeIn fx_importance Method for "ranger" models
#' .
#' @export
fx_importance.ranger <- function(object, v, X,
                                 pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                 n_max = 300L, out_names = NULL, 
                                 w = NULL, sort = TRUE, verbose = TRUE, ...) {
  fx_importance.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    n_max = n_max,
    out_names = out_names,
    w = w,
    sort = sort,
    verbose = verbose,
    ...
  )
}

#' @describeIn fx_importance Method for "mlr3" models.
#' @export
fx_importance.Learner <- function(object, v, X,
                                  pred_fun = function(m, X) m$predict_newdata(X)$response,
                                  n_max = 300L, out_names = NULL, 
                                  w = NULL, sort = TRUE, verbose = TRUE, ...) {
  fx_importance.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    n_max = n_max,
    out_names = out_names,
    w = w,
    sort = sort,
    verbose = verbose,
    ...
  )
}
