#' Calculate Interaction Statistics
#' 
#' @description
#' This is the main function of the package. It does the expensive calculations behind
#' the following H-statistics:
#' - Total interaction strength \eqn{H^2}, a statistic measuring the proportion of
#'   prediction variability unexplained by main effects of `v`, see [h2()] for details.
#' - Friedman and Popescu's statistic \eqn{H^2_j} of overall interaction strength per
#'   feature, see [h2_overall()] for details.
#' - Friedman and Popescu's statistic \eqn{H^2_{jk}} of pairwise interaction strength,
#'   see [h2_pairwise()] for details.
#' - Friedman and Popescu's statistic \eqn{H^2_{jkl}} of three-way interaction strength,
#'   see [h2_threeway()] for details.
#' 
#' Furthermore, it allows to calculate an experimental partial dependence based
#' measure of feature importance, \eqn{\textrm{PDI}_j^2}. It equals the proportion of
#' prediction variability unexplained by other features, see [pd_importance()] 
#' for details. This statistic is not shown by `summary()` or `plot()`.
#'  
#' Instead of using `summary()`, interaction statistics can also be obtained via the 
#' more flexible functions [h2()], [h2_overall()], [h2_pairwise()], and
#' [h2_threeway()].
#'  
#' @param object Fitted model object.
#' @param X A data.frame or matrix serving as background dataset.
#' @param v Vector of feature names, by default `colnames(X)`.
#' @param pred_fun Prediction function of the form `function(object, X, ...)`,
#'   providing \eqn{K \ge 1} predictions per row. Its first argument represents the 
#'   model `object`, its second argument a data structure like `X`. Additional arguments 
#'   (such as `type = "response"` in a GLM, or `reshape = TRUE` in a multiclass XGBoost
#'   model) can be passed via `...`. The default, [stats::predict()], will work in 
#'   most cases.
#' @param n_max If `X` has more than `n_max` rows, a random sample of `n_max` rows is
#'   selected from `X`. In this case, set a random seed for reproducibility.
#' @param w Optional vector of case weights for each row of `X`.
#' @param pairwise_m Number of features for which pairwise statistics are to be 
#'   calculated. The features are selected based on Friedman and Popescu's overall 
#'   interaction strength \eqn{H^2_j}. Set to to 0 to avoid pairwise calculations.
#'   For multivariate predictions, the union of the `pairwise_m` column-wise 
#'   strongest variable names is taken. This can lead to very long run-times.
#' @param threeway_m Like `pairwise_m`, but controls the feature count for 
#'   three-way interactions. Cannot be larger than `pairwise_m`. 
#'   The default is `min(pairwise_m, 5)`. Set to 0 to avoid three-way calculations.
#' @param eps Threshold below which numerator values are set to 0. Default is 1e-10.
#' @param verbose Should a progress bar be shown? The default is `TRUE`.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`, 
#'   for instance `type = "response"` in a [glm()] model.
#' @returns 
#'   An object of class "hstats" containing these elements:
#'   - `X`: Input `X` (sampled to `n_max` rows).
#'   - `w`: Input `w` (sampled to `n_max` values, or `NULL`).
#'   - `v`: Same as input `v`.
#'   - `f`: Matrix with (centered) predictions \eqn{F}.
#'   - `mean_f2`: (Weighted) column means of `f`. Used to normalize \eqn{H^2} and
#'     \eqn{H^2_j}.
#'   - `F_j`: List of matrices, each representing (centered) 
#'     partial dependence functions \eqn{F_j}.
#'   - `F_not_j`: List of matrices with (centered) partial dependence 
#'     functions \eqn{F_{\setminus j}} of other features.
#'   - `K`: Number of columns of prediction matrix.
#'   - `pred_names`: Column names of prediction matrix.
#'   - `pairwise_m`: Like input `pairwise_m`, but capped at `length(v)`.
#'   - `threeway_m`: Like input `threeway_m`, but capped at the smaller of 
#'     `length(v)` and `pairwise_m`.
#'   - `eps`: Like input `eps`.
#'   - `pd_importance`: List with numerator and denominator of \eqn{\textrm{PDI}_j}.
#'   - `h2`: List with numerator and denominator of \eqn{H^2}.
#'   - `h2_overall`: List with numerator and denominator of \eqn{H^2_j}. 
#'   - `v_pairwise`: Subset of `v` with largest \eqn{H^2_j} used for pairwise 
#'     calculations.
#'   - `combs2`: Named list of variable pairs for which pairwise partial 
#'     dependence functions are available.
#'   - `F_jk`: List of matrices, each representing (centered) bivariate 
#'     partial dependence functions \eqn{F_{jk}}.
#'   - `h2_pairwise`: List with numerator and denominator of \eqn{H^2_{jk}}.
#'     Only if pairwise calculations have been done.
#'   - `v_threeway`: Subset of `v` with largest `h2_overall()` used for three-way 
#'     calculations.
#'   - `combs3`: Named list of variable triples for which three-way partial 
#'     dependence functions are available.
#'   - `F_jkl`: List of matrices, each representing (centered) three-way 
#'     partial dependence functions \eqn{F_{jkl}}.
#'   - `h2_threeway`: List with numerator and denominator of \eqn{H^2_{jkl}}.
#' @references
#'   Friedman, Jerome H., and Bogdan E. Popescu. *"Predictive Learning via Rule Ensembles."*
#'     The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
#' @export
#' @seealso [h2()], [h2_overall()], [h2_pairwise()], [h2_threeway()], 
#'   and [pd_importance()] for specific statistics calculated from the resulting object.
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' s <- hstats(fit, X = iris[-1])
#' s
#' plot(s)
#' plot(s, zero = FALSE)  # Drop 0
#' summary(s)
#'   
#' # Absolute pairwise interaction strengths
#' h2_pairwise(s, normalize = FALSE, squared = FALSE, zero = FALSE)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' s <- hstats(fit, X = iris[3:5], verbose = FALSE)
#' plot(s)
#' summary(s)
#'
#' # MODEL 3: Gamma GLM with log link
#' fit <- glm(Sepal.Length ~ ., data = iris, family = Gamma(link = log))
#' 
#' # No interactions for additive features, at least on link scale
#' s <- hstats(fit, X = iris[-1], verbose = FALSE)
#' summary(s)
#' 
#' # On original scale, we have interactions everywhere...
#' s <- hstats(fit, X = iris[-1], type = "response", verbose = FALSE)
#' plot(s, which = 1:3, ncol = 1)  # All three types use different denominators
#' 
#' # All statistics on same scale (of predictions)
#' plot(s, which = 1:3, squared = FALSE, normalize = FALSE, facet_scale = "free_y")
hstats <- function(object, ...) {
  UseMethod("hstats")
}

#' @describeIn hstats Default hstats method.
#' @export
hstats.default <- function(object, X, v = colnames(X),
                           pred_fun = stats::predict, n_max = 300L, 
                           w = NULL, pairwise_m = 5L, 
                           threeway_m = min(pairwise_m, 5L),
                           eps = 1e-10, verbose = TRUE, ...) {
  basic_check(X = X, v = v, pred_fun = pred_fun, w = w)
  p <- length(v)
  stopifnot(p >= 2L)
  pairwise_m <- min(pairwise_m, p)
  threeway_m <- min(threeway_m, pairwise_m, p)
  
  # Reduce size of X (and w)
  if (nrow(X) > n_max) {
    ix <- sample(nrow(X), n_max)
    X <- X[ix, , drop = FALSE]
    if (!is.null(w)) {
      w <- w[ix]
    }
  }
  
  # Predictions ("F" in Friedman and Popescu) always calculated (cheap)
  f <- wcenter(align_pred(pred_fun(object, X, ...)), w = w)
  mean_f2 <- wcolMeans(f^2, w = w)  # A vector
  
  # Initialize first progress bar
  if (verbose) {
    cat("1-way calculations...\n")
    pb <- utils::txtProgressBar(max = p, style = 3)
  }
  
  F_j <- F_not_j <- stats::setNames(vector("list", length = p), v)
  for (j in seq_len(p)) {
    # Main effect of x_j
    z <- v[j]
    g <- if (is.data.frame(X)) X[[z]] else X[, z]
    F_j[[z]] <- wcenter(
      pd_raw(object = object, v = z, X = X, grid = g, pred_fun = pred_fun, w = w, ...),
      w = w
    )
    
    # Total effect of all other features
    not_z <- setdiff(colnames(X), z)
    F_not_j[[z]] <- wcenter(
      pd_raw(
        object, 
        v = not_z, 
        X = X, 
        grid = X[, not_z],
        pred_fun = pred_fun,
        w = w,
        compress_grid = FALSE,  # grid has too many columns (saves a very quick check)
        ...
      ),
      w = w
    )
    
    if (verbose) {
      utils::setTxtProgressBar(pb, j)
    }
  }
  if (verbose) {
    cat("\n")
  }
  
  # Initialize output
  out <- list(
    X = X,
    w = w,
    v = v,
    f = f,
    mean_f2 = mean_f2,
    F_j = F_j, 
    F_not_j = F_not_j, 
    K = ncol(f),
    pred_names = colnames(f),
    pairwise_m = pairwise_m,
    threeway_m = threeway_m,
    eps = eps
  )
  
  # 0-way and 1-way stats
  out[["pd_importance"]] <- pd_importance_raw(out)
  out[["h2"]] <- h2_raw(out)
  out[["h2_overall"]] <- h2_overall_raw(out)
  h2_ov <- out$h2_overall$num
  
  if (pairwise_m >= 2L) {
    out[["v_pairwise"]] <- v2 <- get_v(h2_ov, m = pairwise_m)
    if (length(v2) >= 2L) {
      out[c("combs2", "F_jk")] <- mway(
        object, v = v2, X = X, pred_fun = pred_fun, w = w, way = 2L, verb = verbose, ...
      )
    }
    out[["h2_pairwise"]] <- h2_pairwise_raw(out)
  }
  if (threeway_m >= 3L) {
    out[["v_threeway"]] <- v3 <- get_v(h2_ov, m = threeway_m)
    if (length(v3) >= 3L) {
      out[c("combs3", "F_jkl")] <- mway(
        object, v = v3, X = X, pred_fun = pred_fun, w = w, way = 3L, verb = verbose, ...
      )
    }
    out[["h2_threeway"]] <- h2_threeway_raw(out)
  }

  structure(out, class = "hstats")
}

#' @describeIn hstats Method for "ranger" models.
#' @export
hstats.ranger <- function(object, X, v = colnames(X),
                          pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                          n_max = 300L, w = NULL, pairwise_m = 5L, 
                          threeway_m = min(pairwise_m, 5L),
                          eps = 1e-10, verbose = TRUE, ...) {
  hstats.default(
    object = object,
    X = X,
    v = v,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    pairwise_m = pairwise_m,
    threeway_m = threeway_m,
    eps = eps,
    verbose = verbose,
    ...
  )
}

#' @describeIn hstats Method for "mlr3" models.
#' @export
hstats.Learner <- function(object, X, v = colnames(X),
                           pred_fun = NULL,
                           n_max = 300L, w = NULL, pairwise_m = 5L,
                           threeway_m = min(pairwise_m, 5L), 
                           eps = 1e-10, verbose = TRUE, ...) {
  if (is.null(pred_fun)) {
    pred_fun <- mlr3_pred_fun(object, X = X)
  }
  hstats.default(
    object = object,
    X = X,
    v = v,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    pairwise_m = pairwise_m,
    threeway_m = threeway_m,
    eps = eps,
    verbose = verbose,
    ...
  )
}

#' @describeIn hstats Method for DALEX "explainer".
#' @export
hstats.explainer <- function(object, X = object[["data"]],
                             v = colnames(X),
                             pred_fun = object[["predict_function"]],
                             n_max = 300L, w = object[["weights"]], 
                             pairwise_m = 5L, 
                             threeway_m = min(pairwise_m, 5L),
                             eps = 1e-10, verbose = TRUE, ...) {
  hstats.default(
    object = object[["model"]],
    X = X,
    v = v,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    pairwise_m = pairwise_m,
    threeway_m = threeway_m,
    eps = eps,
    verbose = verbose,
    ...
  )
}

#' Print Method
#' 
#' Print method for object of class "hstats". Shows \eqn{H^2}.
#'
#' @param x An object of class "hstats".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [hstats()] for examples.
print.hstats <- function(x, ...) {
  cat("'hstats' object. Use plot() or summary() for details.\n\n")
  print(h2(x))
  invisible(x)
}

#' Summary Method
#' 
#' Summary method for "hstats" object. Note that only the top 4 overall, the top 3 
#' pairwise and the top 1 three-way statistics are shown.
#'
#' @inheritParams h2_overall
#' @param ... Currently not used.
#' @returns 
#'   An object of class "summary_hstats" representing a named list with statistics
#'   "h2", "h2_overall", "h2_pairwise", "h2_threeway", all of class "hstats_matrix".
#' @export
#' @seealso See [hstats()] for examples.
summary.hstats <- function(object, normalize = TRUE, squared = TRUE, 
                           sort = TRUE, zero = TRUE, ...) {
  args <- list(
    object = object, 
    normalize = normalize, 
    squared = squared, 
    sort = sort,
    zero = zero
  )
  out <- list(
    h2 = do.call(h2, args), 
    h2_overall = do.call(h2_overall, args), 
    h2_pairwise = do.call(h2_pairwise, args), 
    h2_threeway = do.call(h2_threeway, args)
  )
  structure(out, class = "hstats_summary")
}

#' Print Method
#' 
#' Print method for object of class "hstats_summary".
#'
#' @param x An object of class "hstats_summary".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [hstats()] for examples.
print.hstats_summary <- function(x, ...) {
  m <- c(1L, 4L, 3L, 1L)
  
  for (i in seq_along(x)) {
    if (is.null(x[[i]]$M))
      next
    cat("*")
    if (i >= 2L)
      cat("Largest ")
    print(x[[i]], top_m = m[i])
    cat("\n")
  }
  invisible(x)
}

#' Plot Method for "hstats" Object
#' 
#' Plot method for object of class "hstats".
#'
#' @param x Object of class "hstats".
#' @param which Which statistic(s) to be shown? Default is `1:2`, i.e., show both
#'   \eqn{H^2_j} (1) and \eqn{H^2_{jk}} (2). To also show three-way interactions,
#'   use `1:3`.
#' @inheritParams plot.hstats_matrix
#' @inheritParams h2_overall
#' @returns An object of class "ggplot".
#' @export
#' @seealso See [hstats()] for examples.
plot.hstats <- function(x, which = 1:2, normalize = TRUE, squared = TRUE, 
                        sort = TRUE, top_m = 15L, zero = TRUE, fill = "#2b51a1", 
                        facet_scales = "free", ncol = 2L, rotate_x = FALSE, ...) {
  su <- summary(x, normalize = normalize, squared = squared, sort = sort, zero = zero)
  su <- su[sapply(su, FUN = function(z) !is.null(z[["M"]]))]
  
  # This part could be simplified, especially the "match()"
  stat_names <- c("h2_overall", "h2_pairwise", "h2_threeway")[which]
  stat_labs <- c("Overall", "Pairwise", "Three-way")[which]
  ok <- stat_names[stat_names %in% names(su)]
  if (length(ok) == 0L) {
    message("Nothing to plot!")
    return(NULL)
  }
  dat <- lapply(
    ok, 
    FUN = function(nm) 
      mat2df(utils::head(su[[nm]]$M, top_m), id = stat_labs[match(nm, stat_names)])
  )
  dat <- do.call(rbind, dat)
  
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = value_, y = variable_)) +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(su$h2$description)  # Generic enough?
  
  if (length(unique(dat[["id_"]])) > 1L) {
    p <- p + 
      ggplot2::facet_wrap(~ id_, ncol = ncol, scales = facet_scales)
  }
  if (rotate_x) {
    p <- p + rotate_x_labs()
  }
  if (length(unique(dat[["varying_"]])) == 1L) {
    p + ggplot2::geom_bar(fill = fill, stat = "identity", ...)
  } else {
    p + 
      ggplot2::geom_bar(
        ggplot2::aes(fill = varying_), stat = "identity", position = "dodge", ...
      ) + 
      ggplot2::theme(legend.title = ggplot2::element_blank())
  }
}

# Helper functions used only in this script

#' Pairwise or 3-Way Partial Dependencies
#' 
#' Calculates centered partial dependence functions for selected pairwise or three-way
#' situations.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param v Vector of column names to calculate `way` order interactions.
#' @inheritParams hstats
#' @param way Pairwise (`way = 2`) or three-way (`way = 3`) interactions.
#' @param verb Verbose (`TRUE`/`FALSE`).
#' 
#' @returns 
#'   A list with a named list of feature combinations (pairs or triples), and
#'   corresponding centered partial dependencies.
mway <- function(object, v, X, pred_fun = stats::predict, w = NULL, 
                 way = 2L, verb = TRUE, ...) {
  combs <- utils::combn(v, way, simplify = FALSE)
  n_combs <- length(combs)
  F_way <- vector("list", length = n_combs)
  names(F_way) <- names(combs) <- sapply(combs, paste, collapse = ":")
  
  if (verb) {
    cat(way, "way calculations...\n", sep = "-")
    pb <- utils::txtProgressBar(max = n_combs, style = 3)
  }
  
  for (i in seq_len(n_combs)) {
    z <- combs[[i]]
    F_way[[i]] <- wcenter(
      pd_raw(object, v = z, X = X, grid = X[, z], pred_fun = pred_fun, w = w, ...),
      w = w
    )
    if (verb) {
      utils::setTxtProgressBar(pb, i)
    }
  }
  if (verb) {
    cat("\n")
  }
  list(combs, F_way)
}

#' Get Feature Names
#' 
#' This function takes the unsorted and unnormalized H2_j matrix and extracts the top
#' m feature names (unsorted). If H2_j has multiple columns, this is done per column and
#' then the union is returned.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param H Unnormalized, unsorted H2_j values.
#' @param m Number of features to pick per column.
#' 
#' @returns A vector of the union of the m column-wise most important features.
get_v <- function(H, m) {
  v <- rownames(H)
  selector <- function(vv) names(utils::head(sort(-vv[vv > 0]), m))
  if (NCOL(H) == 1L) {
    v_cand <- selector(drop(H))
  } else {
    v_cand <- Reduce(union, lapply(asplit(H, MARGIN = 2L), FUN = selector))
  }
  v[v %in% v_cand]
}
