#' Print Method
#' 
#' Prints the first two rows of the matrix (or matrices) of SHAP values. 
#'
#' @param x An object of class "fastpdp".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' s <- fastpdp(fit, v = "Species", X = iris)
#' s
#' @seealso [fastpdp()]
print.fastpdp <- function(x, ...) {
  cat("Partial dependence profile of dimension", NROW(x$pd), "x", NCOL(x$pd))
  invisible(x)
}

#' Check for fastpdp
#'
#' Is object of class "fastpdp"?
#'
#' @param object An R object.
#' @returns `TRUE` if `object` is of class "fastpdp", and `FALSE` otherwise.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ ., data = iris)
#' s <- fastpdp(fit, v = "Sepal.Width", X = iris, grid = seq(2, 4.4, by = 0.1))
#' is.fastpdp(s)
#' is.fastpdp("a")
#' @seealso [fastpdp()]
is.fastpdp <- function(object){
  inherits(object, "fastpdp")
}
