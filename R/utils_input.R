#' Prepares Predictions
#' 
#' Converts predictions to vector or matrix.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Object representing model predictions.
#' @returns Like `x`, but converted to matrix or vector.
prepare_pred <- function(x) {
  if (is.data.frame(x) && ncol(x) == 1L) {
    x <- x[[1L]]
  }
  if (!is.vector(x) && !is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x) && !is.logical(x)) {
    stop("Predictions must be numeric!")
  }
  return(x)
}

#' Prepares Group BY Variable
#' 
#' Internal function that prepares a BY variable or BY column name.
#' 
#' @noRd
#' @keywords internal
#' @param BY Vector/factor or column name in `X`.
#' @param X Matrix-like.
#' @param by_size Determines if numeric `X` is discrete or needs to be binned.
#' 
#' @returns A list.
prepare_by <- function(BY, X, by_size = 4L) {
  if (length(BY) == 1L && BY %in% colnames(X)) {
    by_name <- BY
    if (is.data.frame(X)) {
      BY <- X[[by_name]]
    } else {
      BY <- X[, by_name]
    }
  } else {
    stopifnot(
      NCOL(BY) == 1L,
      is.vector(BY) || is.factor(BY),
      length(BY) == nrow(X)
    )
    by_name <- "Group"
  }
  
  # Binning
  by_values <- unique(BY)
  if (is.numeric(BY) && length(by_values) > by_size) {
    BY <- qcut(BY, m = by_size)
    by_values <- unique(BY)
  }
  list(BY = BY, by_values = by_values, by_name = by_name)
}

#' Prepares Weight w
#' 
#' Internal function that prepares the weight `w`.
#' 
#' @noRd
#' @keywords internal
#' @param w Vector, or column name in `X`.
#' @param X Matrix-like.
#' 
#' @returns A list.
prepare_w <- function(w, X) {
  if (length(w) == 1L && w %in% colnames(X)) {
    w_name <- w
    w <- if (is.data.frame(X)) X[[w]] else X[, w]
  } else {
    stopifnot(NCOL(w) == 1L, length(w) == nrow(X))
    w_name <- NULL
  }
  if (!is.double(w)) {
    w <- as.double(w)
  }
  list(w = w, w_name = w_name)
}

#' Prepares Response y
#' 
#' Internal function that prepares the response `y`.
#' 
#' @noRd
#' @keywords internal
#' @param y Vector, factor, or matrix-like of the same length as `X`,
#'   or column names in `X`.
#' @param X Matrix-like.
#' 
#' @returns A list with "y" (vector, matrix, or factor) and "y_names" (if `y`
#'   was passed as column names).
prepare_y <- function(y, X) {
  if (NROW(y) < nrow(X) && all(y %in% colnames(X))) {
    y_names <- y
    if (is.data.frame(X) && length(y) == 1L) {
      y <- X[[y]]
    } else {
      y <- X[, y]
    }
  } else {
    if (is.data.frame(y) && ncol(y) == 1L) {
      y <- y[[1L]]
    }
    stopifnot(NROW(y) == nrow(X))
    y_names <- NULL
  }
  if (!is.vector(y) && !is.matrix(y) && !is.factor(y)) {
    y <- as.matrix(y)
  }
  if (!is.numeric(y) && !is.logical(y) && !is.factor(y)) {
    stop("Response must be numeric (or factor.)")
  }
  list(y = y, y_names = y_names)
}

#' Predict Function for Ranger
#' 
#' Returns prediction function for different modes of ranger.
#' 
#' @noRd
#' @keywords internal
#' @param treetype The value of `fit$treetype` in a fitted ranger model.
#' @param survival Cumulative hazards "chf" (default) or probabilities "prob" per time.
#' 
#' @returns A function with signature f(model, newdata, ...).
create_ranger_pred_fun <- function(treetype, survival = c("chf", "prob")) {
  survival <- match.arg(survival)
  
  if (treetype != "Survival") {
    pred_fun <- function(model, newdata, ...) {
      stats::predict(model, newdata, ...)$predictions
    }
    return(pred_fun)
  }

  if (survival == "prob") {
    survival <- "survival"
  }
  
  pred_fun <- function(model, newdata, ...) {
    pred <- stats::predict(model, newdata, ...)
    out <- pred[[survival]]
    colnames(out) <- paste0("t", pred$unique.death.times)
    return(out)
  }
  return(pred_fun)
}

