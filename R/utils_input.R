#' Prepares Predictions
#' 
#' Converts predictions to vector, matrix or factor.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Object representing model predictions.
#' @returns Like `x`, but converted to matrix, vector, or factor.
prepare_pred <- function(x) {
  if (is.data.frame(x)) {
    if (ncol(x) == 1L) {
      x <- x[[1L]]
    } else {
      return(as.matrix(x))
    }
  }
  if (is.vector(x) || is.matrix(x) || is.factor(x)) x else as.matrix(x)
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
prepare_by <- function(BY, X, by_size) {
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
    if (is.data.frame(X)) {
      w <- X[[w]]
    } else {
      w <- X[, w]
    }
  } else {
    stopifnot(
      NCOL(w) == 1L,
      is.numeric(w),    # integer will be ok here
      length(w) == nrow(X)
    )
    w_name <- NULL
  }
  if (!is.double(w)) {  # integer will be converted here
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
#' @param y Vector/matrix-like of the same length as `X`, or column names in `X`.
#' @param X Matrix-like.
#' 
#' @returns A list.
prepare_y <- function(y, X) {
  if (NROW(y) < nrow(X) && all(y %in% colnames(X))) {
    y_names <- y
    if (is.data.frame(X) && length(y) == 1L) {
      y <- X[[y]]
    } else {
      y <- X[, y]
    }
  } else {
    stopifnot(NROW(y) == nrow(X))
    y_names <- NULL
  }
  list(y = y, y_names = y_names)
}

#' mlr3 Helper
#' 
#' Returns the prediction function of a mlr3 Learner.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param object Learner object.
#' @param X Dataframe like object.
#' 
#' @returns A function.
mlr3_pred_fun <- function(object, X) {
  if ("classif" %in% object$task_type) {
    # Check if probabilities are available
    test_pred <- object$predict_newdata(utils::head(X))
    if ("prob" %in% test_pred$predict_types) {
      return(function(m, X) m$predict_newdata(X)$prob)
    } else {
      stop("Set lrn(..., predict_type = 'prob') to allow for probabilistic classification.")
    }
  }
  function(m, X) m$predict_newdata(X)$response
}
