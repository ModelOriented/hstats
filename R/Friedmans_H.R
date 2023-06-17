#' Overall Interaction Strength
#' 
#' Friedman and Popescu's H^2_j of overall interaction strength.
#' 
#' @param object Object of class "interaction_statistics" (or a list of matrices for).
#' @param F_not_j List with matrices containing partial dependence of all variables
#'   except j.
#' @param f Matrix of predictions.
#' @param mean_f2 Weighted average of f^2 values.
#' @param w Optional vector of case weights.
#' @param normalize Should statistic be normalized? Default is `TRUE`.
#' @param squared Should *squared* statistics be returned? Default is `TRUE`. 
#' @param sort Should results be sorted by the size of the statistic? Default is `TRUE`.
#'   Multioutput predictions are sorted by row means.
#' @param top_m How many statistics should be shown? By default `Inf` (show all).
#' @param eps Threshold below which numerator values are set to 0.
#' @param ... Not used.
#' @references
#'   Friedman, Jerome H., and Bogdan E. Popescu. "Predictive Learning via Rule Ensembles."
#'     The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
#' @export
H2_overall <- function(object, ...) {
  UseMethod("H2_overall")
}

#' @describeIn H2_overall Default method of overall interaction strength.
#' @export
H2_overall.default <- function(object, F_not_j, f, mean_f2, w = NULL, 
                               normalize = TRUE, squared = TRUE, sort = TRUE, 
                               top_m = Inf, eps = 1e-8, ...) {
  v <- names(object)
  p <- length(v)
  num <- matrix(nrow = p, ncol = ncol(f), dimnames = list(v, colnames(f)))
  for (i in seq_len(p)) {
    z <- v[i]
    num[i, ] <- wcolMeans((f - object[[z]] - F_not_j[[z]])^2, w = w)
  }
  postprocess(
    num = num,
    denom = mean_f2,
    normalize = normalize, 
    squared = squared, 
    sort = sort, 
    top_m = top_m, 
    eps = eps
  )
}

#' @describeIn H2_overall Overall interaction strength from "interaction_statistics".
#' @export
H2_overall.interaction_statistics <- function(object, normalize = TRUE, squared = TRUE, 
                                              sort = TRUE, top_m = Inf, 
                                              eps = 1e-8, ...) {
  H2_overall.default(
    # statistic
    object = object[["F_j"]],
    F_not_j = object[["F_not_j"]], 
    f = object[["f"]], 
    mean_f2 = object[["mean_f2"]], 
    w = object[["w"]],
    # postprocess
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
#' @param object Object of class "interaction_statistics".
#' @inheritParams H2_overall
#' @inherit H2_overall return references
#' @export
H2_pairwise <- function(object, ...) {
  UseMethod("H2_pairwise")
}

#' @describeIn H2_pairwise Default pairwise interaction strength (not implemented).
#' @export
H2_pairwise.default <- function(object, ...) {
  stop("No default method for H2_pairwise implemented.")
}

#' @describeIn H2_pairwise Pairwise interaction strength from "interaction_statistics".
#' @export
H2_pairwise.interaction_statistics <- function(object, normalize = TRUE, squared = TRUE, 
                                               sort = TRUE, top_m = Inf, 
                                               eps = 1e-8, ...) {
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
    denom[i, ] <- with(object, wcolMeans(F_jk[[i]]^2, w = w))
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

#' Postprocessing of Statistics
#' 
#' Function to apply typical postprocessing steps to a Friedman-Popescu type statistic.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams H2_overall
#' @param num Numerator of statistic.
#' @param denom Denominator of statistic.
postprocess <- function(num, denom = 1, normalize = TRUE, squared = TRUE, 
                        sort = TRUE, top_m = Inf, eps = 1e-8) {
  out <- .zap_small(num, eps = eps)
  if (normalize) {
    out <- out / denom
  }
  if (!squared) {
    out <- sqrt(out)
  }
  if (sort) {
    out <- out[order(-rowSums(out)), , drop = FALSE]
  }
  utils::head(out, n = top_m)
}
