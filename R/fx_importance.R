#' PD Importance
#' 
#' The sample variance of the partial dependence function \eqn{\textrm{PD}_{j}} of 
#' feature \eqn{X^{(j)}} evaluated over a data set \eqn{D} can be used as measure 
#' of **main effect importance**, defined as
#' \deqn{\textrm{PDI}(j, D) = \textrm{Var}\left(\sum_{i \in D} \textrm{PD}_{j}(x_i^{(j)})\right)}.
#' Similarly, we can define the PD importance of two or more features together,
#' measuring their joint effect (main effects plus interaction). Note that Friedman's H
#' (see [fx_interaction()]) uses a similar construction, but there the focus is on pure
#' interaction effects (combined effects minus main effects).
#'  
#' @inheritParams fx_pdp
#' @param v Vector or list of feature names. If passed as list, *vectors* of feature 
#' names are evaluted together. These vectors can be named.
#' @returns
#'   An object of class "fx_importance", containing these elements:
#'   - `imp`: Matrix with importance values per element of `v`.
#'   - `v`: Same as input `v`.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' imp <- fx_importance(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' imp
#' summary(imp)
#' 
#' # With groups of variables
#' v <- list("Sepal.Width", Petal = c("Petal.Width", "Petal.Length"), "Species")
#' imp <- fx_importance(fit, v = v, X = iris, verbose = FALSE)
#' summary(imp, sort = FALSE, out_names = "Importance")
#'
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' summary(imp <- fx_importance(fit, v = v, X = iris))
#' 
#' #' # MODEL THREE: matrix interface
#' X <- model.matrix(Sepal.Length ~ ., data = iris)
#' fit <- lm.fit(x = X, y = iris$Sepal.Length)
#' v <- list("Sepal.Width", "Petal.Length", "Petal.Width", 
#'           Species = c("Speciesversicolor", "Speciesvirginica"))
#' pred_fun <- function(m, x) c(tcrossprod(coef(m), x))
#' imp <- fx_importance(fit, v = v, X = X, pred_fun = pred_fun, verbose = FALSE)
#' summary(imp)
fx_importance <- function(object, ...) {
  UseMethod("fx_importance")
}

#' @describeIn fx_importance Default method.
#' @export
fx_importance.default <- function(object, v, X, pred_fun = stats::predict,
                                  n_max = 300L, w = NULL, verbose = TRUE, ...) {
  p <- length(v)
  stopifnot(
    is.matrix(X) || is.data.frame(X),
    dim(X) >= c(1L, 1L),
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
  
  # Initialize resulting list with good names (move to function...)
  imp <- vector("list", p)
  if (is.null(names(v))) {
    no_names <- rep(TRUE, times = p)
  } else {
    no_names <- names(v) == ""
  }
  if (any(no_names)) {
    names(imp)[no_names] <- sapply(v[no_names], paste, collapse = "*")
  } 
  if (any(!no_names)) {
    names(imp)[!no_names] <- names(v)[!no_names]
  }
  
  for (i in seq_along(imp)) {
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
  structure(list(imp = do.call(rbind, imp), v = v), class = "fx_importance")
}

#' @describeIn fx_importance Method for "ranger" models
#' .
#' @export
fx_importance.ranger <- function(object, v, X,
                                 pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                 n_max = 300L, w = NULL, verbose = TRUE, ...) {
  fx_importance.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' @describeIn fx_importance Method for "mlr3" models.
#' @export
fx_importance.Learner <- function(object, v, X,
                                  pred_fun = function(m, X) m$predict_newdata(X)$response,
                                  n_max = 300L, w = NULL, verbose = TRUE, ...) {
  fx_importance.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
}

#' Importance Print
#' 
#' Print function for result of [fx_importance()].
#' 
#' @inheritParams print.fx_interaction
#' @inherit print.fx_interaction
#' @export
#' @seealso [fx_importance()]
print.fx_importance <- function(x, ...) {
  cat("Importance statistics. Use summary(...) to extract the results.")
  invisible(x)
}

#' Importance Summary
#' 
#' Extracts the results of [fx_importance()].
#' 
#' @inheritParams summary.fx_interaction
#' @inherit summary.fx_interaction returns
#' @export
#' @seealso [fx_importance()]
summary.fx_importance <- function(object, sort = TRUE, out_names = NULL, 
                                  verbose = TRUE, ...) {
  if (verbose) {
    cat("Partial dependence based variable importance\n")
  }
  imp <- fix_names(object[["imp"]], out_names = out_names, prefix = "Imp")
  if (sort) imp[order(-rowSums(imp)), , drop = FALSE] else imp
}


