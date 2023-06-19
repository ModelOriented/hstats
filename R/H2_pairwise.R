#' Pairwise Interaction Strength
#' 
#' Friedman and Popescu's pairwise interaction strength.
#' 
#' @inheritParams pd_raw
#' @inheritParams interact
#' @inheritParams H2_overall
#' @param denominator Should the denominator be the variation of the combined 
#'   effect \eqn{F_{jk}} (like Friedman and Popescu, default) or rather the
#'   variation of the predictions. The latter has the advantage that values can 
#'   directly be compared across variable pairs (and are still relative). 
#'   Only relevant when `normalize = TRUE`.
#' @returns 
#'   Matrix of interactions statistics (one row per variable pair, one column per
#'   prediction dimension).
#' @inherit interact references
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' H2_pairwise(inter)                     # Proportion of pairwise effect variability
#' H2_pairwise(inter, denominator = "f")  # Proportion of prediction variability
#' 
#' \dontrun{
#' H2_pairwise(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' H2_pairwise(inter)
#' H2_pairwise(fit, v = v, X = iris, verbose = FALSE)
#' }
H2_pairwise <- function(object, ...) {
  UseMethod("H2_pairwise")
}

#' @describeIn H2_pairwise Default pairwise interaction strength.
#' @export
H2_pairwise.default <- function(object, v, X, pred_fun = stats::predict,
                                pairwise_m = 5L, n_max = 400L, w = NULL, verbose = TRUE,
                                normalize = TRUE, denominator = c("F_jk", "f"),
                                squared = TRUE, sort = TRUE, 
                                top_m = Inf, eps = 1e-8, ...) {
  istat <- interact(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = pairwise_m,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
  H2_pairwise(
    istat,
    normalize = normalize,
    denominator = denominator,
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}

#' @describeIn H2_pairwise Pairwise interaction strength from "ranger" models.
#' @export
H2_pairwise.ranger <- function(object, v, X, 
                               pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                               pairwise_m = 5L, n_max = 400L, w = NULL, verbose = TRUE,
                               normalize = TRUE, denominator = c("F_jk", "f"),
                               squared = TRUE, sort = TRUE, top_m = Inf, eps = 1e-8, 
                               ...) {
  H2_pairwise.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = pairwise_m,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize,
    denominator = denominator,
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps,
    ...
  )
}

#' @describeIn H2_pairwise Pairwise interaction strength from "mlr3" models.
#' @export
H2_pairwise.Learner <- function(object, v, X, 
                                pred_fun = function(m, X) m$predict_newdata(X)$response,
                                pairwise_m = 5L, n_max = 400L, w = NULL, verbose = TRUE,
                                normalize = TRUE, denominator = c("F_jk", "f"), 
                                squared = TRUE, sort = TRUE, top_m = Inf, eps = 1e-8, 
                                ...) {
  H2_pairwise.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = pairwise_m,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize,
    denominator = denominator,
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps,
    ...
  )
}

#' @describeIn H2_pairwise Pairwise interaction strength from "interact" object.
#' @export
H2_pairwise.interact <- function(object, normalize = TRUE, 
                                 denominator = c("F_jk", "f"),
                                 squared = TRUE, sort = TRUE, 
                                 top_m = Inf, eps = 1e-8, ...) {
  denominator <- match.arg(denominator)
  combs <- object[["combs"]]
  n_combs <- length(combs)
  nms <- colnames(object[["f"]])
  num <- denom <- matrix(
    nrow = n_combs, ncol = length(nms), dimnames = list(names(combs), nms)
  )
  for (i in seq_len(n_combs)) {
    z <- combs[[i]]
    num[i, ] <- with(
      object, wcolMeans((F_jk[[i]] - F_j[[z[1L]]] - F_j[[z[2L]]])^2, w = w)
    )
    denom[i, ] <- with(
      object, 
      if (denominator == "F_jk") wcolMeans(F_jk[[i]]^2, w = w) else mean_f2
    )
  }
  postprocess(
    num = num,
    denom = denom,
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}
