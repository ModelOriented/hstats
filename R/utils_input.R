#' Bin into Quantiles
#' 
#' Internal function. Applies [cut()] to quantile breaks.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x A numeric vector.
#' @param m Number of intervals.
#' @returns A factor, representing binned `x`.
qcut <- function(x, m) {
  p <- seq(0, 1, length.out = m + 1L)
  g <- stats::quantile(x, probs = p, names = FALSE, type = 1L, na.rm = TRUE)
  cut(x, breaks = unique(g), include.lowest = TRUE)
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
    BY <- X[, by_name]
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
    w <- X[, w]
  } else {
    stopifnot(
      NCOL(w) == 1L,
      is.numeric(w),
      length(w) == nrow(X)
    )
    w_name <- NULL
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
    y <- X[, y]
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
