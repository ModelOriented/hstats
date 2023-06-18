#' PD Importance
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
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interaction_statistics(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' pd_importance(inter)
#' pd_importance(inter, main_effects_only = TRUE)
#' 
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
pd_importance.interaction_statistics <- function(object, main_effects_only = FALSE,
                                                 normalize = TRUE, squared = TRUE, 
                                                 sort = TRUE, top_m = Inf, 
                                                 eps = 1e-8, ...) {
  num <- with(
    object, 
    matrix(nrow = length(v), ncol = ncol(f), dimnames = list(v, colnames(f)))
  )
  for (z in rownames(num)) {
    temp <- with(object, if (main_effects_only) F_j[[z]] else f - F_not_j[[z]])
    num[z, ] <- wcolMeans(temp^2, w = object[["w"]])
  }
  postprocess(
    num = num,
    denom = object[["mean_f2"]],
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}

