#' Overall Interaction Strength
#' 
#' Friedman and Popescu's H^2_j of overall interaction strength.
#' 
#' @param v Vector of column names.
#' @param f Matrix of predictions.
#' @param F_j List of univariate PD.
#' @param F_not_j List of PDs of all variables != j.
#' @param w Optional case weights.
#' @param eps Threshold below which to set numerator values to 0.
#' @returns Statistic
get_H2_j <- function(v, f, F_j, F_not_j, w = NULL, eps = 1e-8) {
  out <- stats::setNames(vector("list", length = length(v)), v)
  for (z in v) {
    out[[z]] <- wcolMeans((f - F_j[[z]] - F_not_j[[z]])^2, w = w)
  }
  .zap_small(do.call(rbind, out), eps = eps) / wcolMeans(f^2, w = w)
}

#' Friedman and Popescu's Pairwise Interaction Strength
#' 
#' @param object Object of class "interaction".
#' @param normalize Should statistic be normalized? Default is `TRUE`.
#' @param squared Should squared statistics be returned? Default is `TRUE`. 
#' @param sort Should results be sorted by the size of the statistic? Default is `TRUE`.
#'   Multioutput predictions are sorted by row means.
#' @param top_m How many statistics should be shown? By default `Inf` (show all).
#' @param out_names Optional names of the output columns corresponding to the 
#'   K-dimensional predictions.
#' @param eps Threshold below which numerator values are set to 0.
#' @return Matrix with statistics.
#' @references
#'   Friedman, Jerome H., and Bogdan E. Popescu. "Predictive Learning via Rule Ensembles."
#'     The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
#' @export
H2_jk <- function(object, normalize = TRUE, squared = TRUE, sort = TRUE, 
                  top_m = Inf, out_names = NULL, eps = 1e-8) {
  stopifnot(
    inherits(object, "interaction"),
    length(object[["F_jk"]]) >= 1L
  )
  postprocess(
    S = H2_jk_raw(object), 
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    out_names = out_names,
    eps = eps
  )
}

#' Friedman and Popescu's Overall Interaction Strength
#' 
#' @inheritParams H2_jk
#' @inherit H2_jk references return
H2_j <- function(object, normalize = TRUE, squared = TRUE, sort = TRUE, 
                 top_m = Inf, out_names = NULL, eps = 1e-8) {
  stopifnot(
    inherits(object, "interaction"),
    length(object[["F_j"]]) >= 1L
  )
  postprocess(
    S = H2_j_raw(object), 
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    out_names = out_names,
    eps = eps
  )
}
  
# Helper functions

#' Raw Pairwise Interaction Strength
#' 
#' @noRd
#' 
#' @inheritParams H2_jk
#' @returns List with `num`erator and `denom`inator used to calculate statistics.
H2_jk_raw <- function(object) {
  combs <- object[["combs"]]
  p <- length(combs)
  num <- denom <- matrix(
    nrow = p, 
    ncol = object[["K"]], 
    dimnames = list(names(combs), object[["pred_names"]])
  )
  for (i in seq_len(p)) {
    z <- combs[[i]]
    num[i, ] <- with(
      object, wcolMeans((F_jk[[i]] - F_j[[z[1L]]] - F_j[[z[2L]]])^2, w = w)
    )
    denom[i, ] <- with(object, wcolMeans(F_jk[[i]]^2, w = w))
  }
  list(num = num, denom = denom)
}

#' Raw Overall Interaction Strength
#' 
#' @noRd
#' 
#' @inheritParams H2_jk
#' @inherit return H2_jk_raw
H2_j_raw <- function(object) {
  v <- object[["v"]]
  p <- length(v)
  num <- matrix(
    nrow = p, ncol = object[["K"]], dimnames = list(v, object[["pred_names"]])
  )
  for (i in seq_len(p)) {
    z <- v[i]
    num[i, ] <- with(object, wcolMeans((f - F_j[[z]] - F_not_j[[z]])^2, w = w))
  }
  list(num = num, denom = object[["mean_f_squared"]])
}

  