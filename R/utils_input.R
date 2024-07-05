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
  if (!is.vector(x) || !is.matrix(x)) {
    x <- as.matrix(x)
  }
  if (!is.numeric(x)) {
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
#' @param ohe If y is a factor: should it be one-hot encoded? Default is `FALSE`.
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
    stopifnot(NROW(y) == nrow(X))
    y_names <- NULL
  }
  list(y = prepare_pred(y), y_names = y_names)
}

