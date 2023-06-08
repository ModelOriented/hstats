#' PD Importance
#' 
#' The sample variance of the partial dependence function \eqn{\textrm{PD}_{j}} of 
#' feature \eqn{X^{(j)}} evaluated over a data set \eqn{D} can be used as measure 
#' of **main effect importance**, defined as
#' \deqn{\textrm{PDI}(j, D) = \textrm{Var}\left(\sum_{i \in D} \textrm{PD}_{j}(x_i^{(j)})\right)}.
#' Similarly, we can define the PD importance of two or more features together,
#' measuring their joint effect (main effects plus interaction). Note that Friedman's H
#' (see [pd_interaction()]) uses a similar construction, but there the focus is on pure
#' interaction effects (combined effects minus main effects).
#'  
#' @inheritParams pd_raw
#' @param v Vector or list of feature names. If passed as list, *vectors* of feature 
#'   names are evaluted together. These vectors can be named for better readability of
#'   results.
#' @param verbose Should a progress bar be shown? The default is `TRUE`.
#' @returns
#'   An object of class "pd_importance", containing these elements:
#'   - `imp`: Matrix with importance values per element of `v`.
#'   - `v`: Same as input `v`.
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' imp <- pd_importance(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' imp
#' summary(imp)
#' 
#' # With groups of variables
#' v <- list("Sepal.Width", Petal = c("Petal.Width", "Petal.Length"), "Species")
#' imp <- pd_importance(fit, v = v, X = iris, verbose = FALSE)
#' summary(imp, out_names = "Importance")
#'
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width + Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' summary(imp <- pd_importance(fit, v = v, X = iris))
#' 
#' # MODEL THREE: matrix interface
#' X <- model.matrix(Sepal.Length ~ ., data = iris)
#' fit <- lm.fit(x = X, y = iris$Sepal.Length)
#' v <- list("Sepal.Width", "Petal.Length", "Petal.Width", 
#'           Species = c("Speciesversicolor", "Speciesvirginica"))
#' pred_fun <- function(m, x) c(tcrossprod(coef(m), x))
#' imp <- pd_importance(fit, v = v, X = X, pred_fun = pred_fun, verbose = FALSE)
#' summary(imp)
pd_importance <- function(object, ...) {
  UseMethod("pd_importance")
}

#' @describeIn pd_importance Default method.
#' @export
pd_importance.default <- function(object, v, X, pred_fun = stats::predict,
                                  n_max = 300L, w = NULL, verbose = TRUE, ...) {
  .basic_check(
    X = X, v = unique(unlist(v, use.names = FALSE)), pred_fun = pred_fun, w = w
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
  p <- length(v)
  show_bar <- verbose && p >= 2L
  if (show_bar) {
    pb <- utils::txtProgressBar(1L, p, style = 3)
  }
  
  # Initialize resulting list with good names
  imp <- vector("list", length = p)
  if (is.null(names(v))) {
    names(imp) <- sapply(v, paste, collapse = "*")
  } else {
    names(imp) <- ifelse(names(v) == "", sapply(v, paste, collapse = "*"), names(v))
  }
  
  for (i in seq_along(imp)) {
    z <- v[[i]]
    g <- if (is.data.frame(X) && length(z) == 1L) X[[z]] else X[, z]
    pd <- pd_raw(
      object = object, 
      v = z, 
      X = X, 
      grid = g,
      pred_fun = pred_fun,
      n_max = n_max, # No effect
      w = w,
      check = FALSE, # Already done
      ...
    )
    imp[[i]] <- apply(pd, 2L, FUN = stats::var)
    if (show_bar) {
      utils::setTxtProgressBar(pb, i)
    }
  } 
  if (show_bar) {
    cat("\n")
  }
  structure(list(imp = do.call(rbind, imp), v = v), class = "pd_importance")
}

#' @describeIn pd_importance Method for "ranger" models
#' .
#' @export
pd_importance.ranger <- function(object, v, X,
                                 pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                                 n_max = 300L, w = NULL, verbose = TRUE, ...) {
  pd_importance.default(
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

#' @describeIn pd_importance Method for "mlr3" models.
#' @export
pd_importance.Learner <- function(object, v, X,
                                  pred_fun = function(m, X) m$predict_newdata(X)$response,
                                  n_max = 300L, w = NULL, verbose = TRUE, ...) {
  pd_importance.default(
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
#' Print function for result of [pd_importance()].
#' 
#' @inheritParams print.pd
#' @inherit print.pd return
#' @export
#' @seealso [pd_importance()]
print.pd_importance <- function(x, ...) {
  cat("Importance statistics. Use summary(...) to extract the results.")
  invisible(x)
}

#' Importance Summary
#' 
#' Extracts the results of [pd_importance()].
#' 
#' @inheritParams summary.pd
#' @inheritParams summary.pd_interaction
#' @inherit summary.pd_interaction return
#' @export
#' @seealso [pd_importance()]
summary.pd_importance <- function(object, sort = TRUE, top_m = Inf,
                                  out_names = NULL, verbose = TRUE, ...) {
  if (verbose) {
    cat("Partial dependence based variable importance\n")
  }
  imp <- fix_names(object[["imp"]], out_names = out_names, prefix = "Imp")
  if (sort) {
    imp <- imp[order(-rowSums(imp)), , drop = FALSE] 
  }
  utils::head(imp, n = top_m)
}


