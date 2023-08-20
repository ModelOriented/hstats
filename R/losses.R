#' Internal function. Squared error loss
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix.
#' @param predicted A numeric vector/matrix.
#' @returns Vector or matrix of losses.
loss_squared_error <- function(actual, predicted) {
  check_dim(actual = actual, predicted = predicted)
  (actual - predicted)^2
}

#' Internal function. Absolute error loss
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix.
#' @param predicted A numeric vector/matrix.
#' @returns Vector or matrix of losses.
loss_absolute_error <- function(actual, predicted) {
  check_dim(actual = actual, predicted = predicted)
  abs(actual - predicted)
}

#' Internal function. Poisson deviance loss
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix with non-negative values.
#' @param predicted A numeric vector/matrix with non-negative values.
#' @returns Vector or matrix of losses.
loss_poisson <- function(actual, predicted) {
  check_dim(actual = actual, predicted = predicted)
  stopifnot(
    all(predicted >= 0),
    all(actual >= 0)
  )
  out <- predicted
  p <- actual > 0
  out[p] <- (actual * log(actual / predicted) - (actual  - predicted))[p]
  2 * out
}

#' Internal function. Gamma deviance loss
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix with positive values.
#' @param predicted A numeric vector/matrix with positive values.
#' @returns Vector or matrix of losses.
loss_gamma <- function(actual, predicted) {
  check_dim(actual = actual, predicted = predicted)
  stopifnot(
    all(predicted > 0), 
    all(actual > 0)
  )
  -2 * (log(actual / predicted) - (actual - predicted) / predicted)
}

#' Internal function. logloss
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix with values between 0 and 1.
#' @param predicted A numeric vector/matrix with values between 0 and 1.
#' @returns Vector or matrix of losses.
loss_logloss <- function(actual, predicted) {
  check_dim(actual = actual, predicted = predicted)
  stopifnot(
    all(predicted >= 0), 
    all(predicted <= 1),
    all(actual >= 0),
    all(actual <= 1)
  )
  -xlogy(actual, predicted) - xlogy(1 - actual, 1 - predicted)
}

#' Internal function. mlogloss
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix with values between 0 and 1.
#' @param predicted A numeric vector/matrix with values between 0 and 1.
#' @returns `TRUE` (or an error message).
loss_mlogloss <- function(actual, predicted) {
  check_dim(actual = actual, predicted = predicted)
  stopifnot(
    all(predicted >= 0), 
    all(predicted <= 1),
    all(actual >= 0),
    all(actual <= 1)
  )
  -rowSums(xlogy(actual, predicted))
}

#' Internal function. Checks if dimensions of vectors/matrices are identical.
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix.
#' @param predicted A numeric vector/matrix.
#' @returns `TRUE` (or an error message).
check_dim <- function(actual, predicted) {
  stopifnot(
    NROW(actual) == NROW(predicted),
    NCOL(actual) == NCOL(predicted)
  )
  TRUE
}

#' Internal function. Calculates x*log(y)
#' 
#' @noRd
#' @keywords internal
#'
#' @param x A numeric vector/matrix.
#' @param y A numeric vector/matrix.
#' @returns A numeric vector or matrix.
xlogy <- function(x, y) {
  out <- x
  p <- x != 0
  out[p] <- x[p] * log(y[p])
  out
}
