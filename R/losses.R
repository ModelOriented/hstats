#' Input Checks for Losses
#' 
#' Internal function with general input checks.
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual Actual values.
#' @param predicted Predictions.
#' @returns `TRUE`
check_loss <- function(actual, predicted) {
  stopifnot(
    is.vector(actual) || is.matrix(actual),
    is.vector(predicted) || is.matrix(predicted),
    is.numeric(actual) || is.logical(actual),
    is.numeric(predicted) || is.logical(predicted),
    NROW(actual) == NROW(predicted),
    NCOL(actual) == 1L || NCOL(actual) == NCOL(predicted)
  )
  return(TRUE)
}

#' Squared Error Loss
#' 
#' Internal function. Calculates squared error.
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector or matrix, or a factor with levels in the same order 
#'   as the column names of `predicted`.
#' @param predicted A numeric vector or matrix.
#' @returns Vector or matrix of numeric losses.
loss_squared_error <- function(actual, predicted) {
  if (is.factor(actual)) {
    actual <- fdummy(actual)
  }
  check_loss(actual, predicted)
  
  return((drop(actual) - predicted)^2)
}

#' Absolute Error Loss
#' 
#' Internal function. Calculates absolute error.
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix.
#' @param predicted A numeric vector/matrix.
#' @returns Vector or matrix of numeric losses.
loss_absolute_error <- function(actual, predicted) {
  check_loss(actual, predicted)

  return(abs(drop(actual) - predicted))
}

#' Poisson Deviance Loss
#' 
#' Internal function. Calculates Poisson deviance.
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix with non-negative values.
#' @param predicted A numeric vector/matrix with non-negative values.
#' @returns Vector or matrix of numeric losses.
loss_poisson <- function(actual, predicted) {
  check_loss(actual, predicted)
  stopifnot(
    all(predicted >= 0),
    all(actual >= 0)
  )
  
  actual <- drop(actual)
  
  out <- predicted
  p <- actual > 0
  out[p] <- (actual * log(actual / predicted) - (actual  - predicted))[p]
  return(2 * out)
}

#' Gamma Deviance Loss
#' 
#' Internal function. Calculates Gamma deviance.
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix with positive values.
#' @param predicted A numeric vector/matrix with positive values.
#' @returns Vector or matrix of numeric losses.
loss_gamma <- function(actual, predicted) {
  check_loss(actual, predicted)
  stopifnot(
    all(predicted > 0), 
    all(actual > 0)
  )
  
  actual <- drop(actual)
  
  return(-2 * (log(actual / predicted) - (actual - predicted) / predicted))
}

#' Log Loss
#' 
#' Internal function. Calculates log loss, which is equivalent to binary cross-entropy 
#' and half the Bernoulli deviance.
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric vector/matrix with values between 0 and 1.
#' @param predicted A numeric vector/matrix with values between 0 and 1.
#' @returns Vector or matrix of numeric losses.
loss_logloss <- function(actual, predicted) {
  check_loss(actual, predicted)
  stopifnot(
    all(predicted >= 0), 
    all(predicted <= 1),
    all(actual >= 0),
    all(actual <= 1)
  )
  
  actual <- drop(actual)
  
  return(-xlogy(actual, predicted) - xlogy(1 - actual, 1 - predicted))
}

#' Multi-Column Log Loss
#' 
#' Internal function. Log loss for probabilistic classification models with more than
#' one prediction column (one per category).
#' 
#' @noRd
#' @keywords internal
#'
#' @param actual A numeric matrix with values between 0 and 1, or a 
#'   factor, or a discrete numeric vector that will be one-hot-encoded by a 
#'   fast version of `model.matrix(~ as.factor(actual) + 0)`.
#'   The column order of `predicted` must be in line with this!
#' @param predicted A numeric matrix with values between 0 and 1.
#' @returns A numeric vector of losses.
loss_mlogloss <- function(actual, predicted) {
  if (NCOL(actual) == 1L) {  # not only for factors
    actual <- fdummy(actual)
  }
  
  stopifnot(
    is.matrix(actual),
    is.matrix(predicted),
    
    is.numeric(actual) || is.logical(actual),
    is.numeric(predicted) || is.logical(predicted),
    
    dim(actual) == dim(predicted),
    ncol(predicted) >= 2L,
    
    all(predicted >= 0), 
    all(predicted <= 1),
    all(actual >= 0),
    all(actual <= 1)
  )
  
  return(unname(-rowSums(xlogy(actual, predicted))))
}

#' Calculates x*log(y)
#' 
#' Internal function. Returns 0 whenever x = 0 and y >= 0.
#' 
#' @noRd
#' @keywords internal
#'
#' @param x A numeric vector/matrix.
#' @param y A numeric vector/matrix.
#' @returns A numeric vector or matrix.
xlogy <- function(x, y) {
  out <- x * log(y)
  out[x == 0] <- 0
  
  return(out)
}

#' String to function
#' 
#' Internal function that turns a string like "squared_error" into the corresponding
#' loss function.
#' 
#' @noRd
#' @keywords internal
#'
#' @param loss A string.
#' @returns A function.
get_loss_fun <- function(loss) {
  switch(
    loss,
    squared_error = loss_squared_error,
    logloss = loss_logloss,
    mlogloss = loss_mlogloss,
    poisson = loss_poisson,
    gamma = loss_gamma,
    absolute_error = loss_absolute_error,
    stop("Unknown loss function.")
  )
}

