#' Overall Interaction Strength
#' 
#' Friedman and Popescu's H^2_j of overall interaction strength.
#' 
#' @inheritParams interaction_statistics
#' @param object Object of class "interaction_statistics", or a model object.
#' @param normalize Should statistic be normalized? Default is `TRUE`.
#' @param squared Should *squared* statistics be returned? Default is `TRUE`. 
#' @param sort Should results be sorted by the size of the statistic? Default is `TRUE`.
#'   Multioutput predictions are sorted by row means.
#' @param top_m How many statistics should be shown? By default `Inf` (show all).
#' @param eps Threshold below which numerator values are set to 0.
#' @param ... Further parameters passed to predict function (only for default method).
#' @inherit interaction_statistics references
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interaction_statistics(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' H2_overall(inter)
#' 
#' \dontrun{
#' H2_overall(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interaction_statistics(fit, v = v, X = iris, verbose = FALSE)
#' H2_overall(inter)
#' H2_overall(fit, v = v, X = iris, verbose = FALSE)
#' }
H2_overall <- function(object, ...) {
  UseMethod("H2_overall")
}

#' @describeIn H2_overall Default method of overall interaction strength.
#' @export
H2_overall.default <- function(object, v, X, pred_fun = stats::predict,
                               n_max = 300L, w = NULL, verbose = TRUE,
                               normalize = TRUE, squared = TRUE, sort = TRUE, 
                               top_m = Inf, eps = 1e-8, ...) {
  
  istat <- interaction_statistics(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = 0L,
    n_max = n_max,
    w = w,
    verbose = verbose,
    ...
  )
  H2_overall(
    istat,
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}

#' @describeIn H2_overall Overall interaction strength from "ranger" models.
#' @export
H2_overall.ranger <- function(object, v, X, 
                              pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                              n_max = 300L, w = NULL, verbose = TRUE,
                              normalize = TRUE, squared = TRUE, sort = TRUE, 
                              top_m = Inf, eps = 1e-8, ...) {
  H2_overall.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = 0L,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps,
    ...
  )
}

#' @describeIn H2_overall Overall interaction strength from "mlr3" models.
#' @export
H2_overall.Learner <- function(object, v, X, 
                               pred_fun = function(m, X) m$predict_newdata(X)$response,
                               n_max = 300L, w = NULL, verbose = TRUE,
                               normalize = TRUE, squared = TRUE, sort = TRUE, 
                               top_m = Inf, eps = 1e-8, ...) {
  H2_overall.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    pairwise_m = 0L,
    n_max = n_max,
    w = w,
    verbose = verbose,
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps,
    ...
  )
}

#' @describeIn H2_overall Overall interaction strength from "interaction_statistics".
#' @export
H2_overall.interaction_statistics <- function(object, normalize = TRUE, squared = TRUE, 
                                              sort = TRUE, top_m = Inf, 
                                              eps = 1e-8, ...) {
  H2_overall_raw(
    F_j = object[["F_j"]],
    F_not_j = object[["F_not_j"]], 
    f = object[["f"]], 
    mean_f2 = object[["mean_f2"]], 
    w = object[["w"]],
    normalize = normalize,
    squared = squared,
    sort = sort,
    top_m = top_m,
    eps = eps
  )
}

#' Pairwise Interaction Strength
#' 
#' Friedman and Popescu's Pairwise Interaction Strength.
#' 
#' @param object Object of class "interaction_statistics" (or a model).
#' @param denominator Should the denominator be based on the variation of the combined 
#'   effect \eqn{F_{jk}} (in line with Friedman and Popescu's H statistic) or of the
#'   variation of the predictions. The latter can be directly compared across variable
#'   pairs. This option is relevant only if `normalize = TRUE`.
#' @inheritParams H2_overall
#' @inheritParams interaction_statistics
#' @inherit H2_overall return references
#' @export
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interaction_statistics(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' H2_pairwise(inter)                     # Proportion of pairwise effect variability
#' H2_pairwise(inter, denominator = "f")  # Proportion of prediction variability
#' 
#' \dontrun{
#' H2_pairwise(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interaction_statistics(fit, v = v, X = iris, verbose = FALSE)
#' H2_pairwise(inter)
#' H2_pairwise(fit, v = v, X = iris, verbose = FALSE)
#' }
H2_pairwise <- function(object, ...) {
  UseMethod("H2_pairwise")
}

#' @describeIn H2_pairwise Default pairwise interaction strength.
#' @export
H2_pairwise.default <- function(object, v, X, pred_fun = stats::predict,
                                pairwise_m = 5L, n_max = 300L, w = NULL, verbose = TRUE,
                                normalize = TRUE, denominator = c("F_jk", "f"),
                                squared = TRUE, sort = TRUE, 
                                top_m = Inf, eps = 1e-8, ...) {
  istat <- interaction_statistics(
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
                               pairwise_m = 5L, n_max = 300L, w = NULL, verbose = TRUE,
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
                                pairwise_m = 5L, n_max = 300L, w = NULL, verbose = TRUE,
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

#' @describeIn H2_pairwise Pairwise interaction strength from "interaction_statistics".
#' @export
H2_pairwise.interaction_statistics <- function(object, normalize = TRUE, 
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
