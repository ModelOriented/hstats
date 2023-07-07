#' Calculate Interaction Statistics
#' 
#' @description
#' This is the main function of the package. It does the expensive calculations behind
#' these interaction statistics:
#' - Total interaction strength \eqn{H^2}, a statistic measuring the proportion of
#'   prediction variability unexplained by main effects of `v`, see [H2()] for details.
#' - Friedman and Popescu's \eqn{H^2_j} statistic of overall interaction strength per
#'   feature, see [H2_overall()] for details.
#' - Friedman and Popescu's \eqn{H^2_{jk}} statistic of pairwise interaction strength,
#'   see [H2_pairwise()] for details.
#' - Friedman and Popescu's \eqn{H^2_{jkl}} statistic of three-way interaction strength,
#'   see [H2_threeway()] for details.
#' 
#' Furthermore, it allows to calculate an experimental partial dependence based
#' measure of feature importance, \eqn{\textrm{PDI}_j^2}. It equals the proportion of
#' prediction variability unexplained by other features, see [pd_importance()] 
#' for details. (This statistic is not shown by `summary()` or `plot()`.) 
#'  
#' Instead of using `summary()`, interaction statistics can also be obtained via the 
#' more flexible functions [H2()], [H2_overall()], [H2_pairwise()], and
#' [H2_threeway()].
#'  
#' @param object Fitted model object.
#' @param v Vector of feature names.
#' @param X A data.frame or matrix serving as background dataset.
#' @param pred_fun Prediction function of the form `function(object, X, ...)`,
#'   providing K >= 1 numeric predictions per row. Its first argument represents the 
#'   model `object`, its second argument a data structure like `X`. Additional arguments 
#'   (such as `type = "response"` in a GLM) can be passed via `...`. The default, 
#'   [stats::predict()], will work in most cases. Note that column names in a resulting
#'   matrix of predictions will be used as default column names in the results.
#' @param n_max If `X` has more than `n_max` rows, a random sample of `n_max` rows is
#'   selected from `X`. In this case, set a random seed for reproducibility.
#' @param w Optional vector of case weights for each row of `X`.
#' @param pairwise_m Number of features for which pairwise statistics are to be 
#'   calculated. The features are selected based on Friedman and Popescu's overall 
#'   interaction strength \eqn{H^2_j}. 
#'   Set to `length(v)` to calculate every pair and to 0 to avoid pairwise calculations.
#'   For multivariate predictions, the union of the column-wise strongest variable
#'   names is taken. This can lead to very long run-times.
#' @param threeway_m Same as `pairwise_m`, but controlling the number of features for
#'   which threeway interactions should be calculated. Not larger than `pairwise_m`.
#' @param verbose Should a progress bar be shown? The default is `TRUE`.
#' @param ... Additional arguments passed to `pred_fun(object, X, ...)`, 
#'   for instance `type = "response"` in a [glm()] model.
#' @returns 
#'   An object of class "interact" containing these elements:
#'   - `X`: Input `X` (sampled to `n_max` rows).
#'   - `w`: Input `w` (sampled to `n_max` values, or `NULL`).
#'   - `v`: Same as input `v`.
#'   - `f`: Matrix with (centered) predictions \eqn{F}.
#'   - `mean_f2`: (Weighted) column means of `f`. Used to normalize most statistics.
#'   - `F_j`: List of matrices, each representing (centered) 
#'     partial dependence functions \eqn{F_j}.
#'   - `F_not_j`: List of matrices with (centered) partial dependence 
#'     functions \eqn{F_{\setminus j}} of other features.
#'   - `K`: Number of columns of prediction matrix.
#'   - `pred_names`: Column names of prediction matrix.
#'   - `v_pairwise`: Subset of `v` with largest `H2_overall` used for pairwise 
#'     calculations.
#'   - `combs2`: Named list of variable pairs for which pairwise partial 
#'     dependence functions are available.
#'   - `F_jk`: List of matrices, each representing (centered) bivariate 
#'     partial dependence functions \eqn{F_{jk}}.
#'   - `v_threeway`: Subset of `v` with largest `H2_overall` used for three-way 
#'     calculations.
#'   - `combs3`: Named list of variable triples for which three-way partial 
#'     dependence functions are available.
#'   - `F_jkl`: List of matrices, each representing (centered) three-way 
#'     partial dependence functions \eqn{F_{jkl}}.
#' @references
#'   Friedman, Jerome H., and Bogdan E. Popescu. *"Predictive Learning via Rule Ensembles."*
#'     The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
#' @export
#' @seealso [H2()], [H2_overall()], [H2_pairwise()] and [H2_threeway()] for specific 
#'   statistics calculated from the resulting object.
#' @examples
#' # MODEL 1: Linear regression
#' fit <- lm(
#'   Sepal.Length ~ Petal.Width*Species*Petal.Length, 
#'   data = iris
#' )
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' inter
#' plot(inter)
#' summary(inter)
#'   
#' # Absolute pairwise interaction strengths
#' H2_pairwise(inter, normalize = FALSE, squared = FALSE, plot = FALSE)
#' 
#' # MODEL 2: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' plot(inter)
#' summary(inter)
#'
#' # MODEL 3: Gamma GLM with log link
#' fit <- glm(Sepal.Length ~ ., data = iris, family = Gamma(link = log))
#' 
#' # No interactions for additive features, at least on link scale
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' summary(inter)
#' 
#' # On original scale, we have interactions everywhere...
#' inter <- interact(
#'   fit, v = names(iris[-1]), X = iris, type = "response", verbose = FALSE
#' )
#' plot(inter)
interact <- function(object, ...) {
  UseMethod("interact")
}

#' @describeIn interact Default interact method.
#' @export
interact.default <- function(object, v, X, pred_fun = stats::predict, n_max = 300L, 
                             w = NULL, pairwise_m = 5L, threeway_m = pairwise_m,
                             verbose = TRUE, ...) {
  basic_check(X = X, v = v, pred_fun = pred_fun, w = w)
  stopifnot(threeway_m <= pairwise_m)
  
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
  p <- length(v)
  show_bar <- verbose && p >= 2L
  if (show_bar) {
    cat("1-way calculations...\n")
    pb <- utils::txtProgressBar(1L, max = p, style = 3)
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
    
    if (show_bar) {
      utils::setTxtProgressBar(pb, j)
    }
  }
  if (show_bar) {
    cat("\n")
  }
  
  # Initialize output
  out <- structure(
    list(
      X = X,
      w = w,
      v = v,
      f = f,
      mean_f2 = mean_f2,
      F_j = F_j, 
      F_not_j = F_not_j, 
      K = ncol(f),
      pred_names = colnames(f),
      v_pairwise = NULL,
      combs2 = NULL,
      F_jk = NULL,
      v_threeway = NULL,
      combs3 = NULL,
      F_jkl = NULL
    ), 
    class = "interact"
  )
  
  # 2+way stats are calculated only for subset of features with large interactions
  H <- H2_overall(out, normalize = FALSE, sort = FALSE, plot = FALSE)
  
  out[["v_pairwise"]] <- v2 <- get_v(H, m = pairwise_m)
  if (min(pairwise_m, length(v2)) >= 2L) {
    out[c("combs2", "F_jk")] <- mway(
      object, v = v2, X = X, pred_fun = pred_fun, w = w, way = 2L, verb = verbose, ...
    )
  }
  
  # Threeway interactions
  out[["v_threeway"]] <- v3 <- get_v(H, m = threeway_m)
  if (min(threeway_m, length(v3)) >= 3L) {
    out[c("combs3", "F_jkl")] <- mway(
      object, v = v3, X = X, pred_fun = pred_fun, w = w, way = 3L, verb = verbose, ...
    )
  }
  out
}

#' @describeIn interact Method for "ranger" models.
#' @export
interact.ranger <- function(object, v, X,
                            pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                            n_max = 300L, w = NULL, pairwise_m = 5L, 
                            verbose = TRUE, ...) {
  interact.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    pairwise_m = pairwise_m,
    verbose = verbose,
    ...
  )
}

#' @describeIn interact Method for "mlr3" models.
#' @export
interact.Learner <- function(object, v, X,
                             pred_fun = function(m, X) m$predict_newdata(X)$response,
                             n_max = 300L, w = NULL, pairwise_m = 5L,
                             verbose = TRUE, ...) {
  interact.default(
    object = object,
    v = v,
    X = X,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    pairwise_m = pairwise_m,
    verbose = verbose,
    ...
  )
}

#' @describeIn interact Method for DALEX "explainer".
#' @export
interact.explainer <- function(object, v = colnames(object[["data"]]), 
                               X = object[["data"]],
                               pred_fun = object[["predict_function"]],
                               n_max = 300L, w = object[["weights"]], 
                               pairwise_m = 5L, verbose = TRUE, ...) {
  interact.default(
    object = object[["model"]],
    v = v,
    X = X,
    pred_fun = pred_fun,
    n_max = n_max,
    w = w,
    pairwise_m = pairwise_m,
    verbose = verbose,
    ...
  )
}

#' Print Method
#' 
#' Print method for object of class "interact". Shows \eqn{H^2} statistic.
#'
#' @param x An object of class "interact".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @seealso See [interact()] for examples.
print.interact <- function(x, ...) {
  cat("'interact' object. Run plot() or summary() for details.\n\n")
  cat("Proportion of prediction variability unexplained by main effects of v:\n")
  print(H2(x))
  cat("\n")
  invisible(x)
}

#' Summary Method
#' 
#' Summary method for "interact" object.
#'
#' @param object An object of class "interact".
#' @param top_m Maximum number of rows of results to print.
#' @param ... Further arguments passed to statistics, e.g., `normalize = FALSE`.
#' @returns A named list of statistics.
#' @export
#' @seealso See [interact()] for examples.
summary.interact <- function(object, top_m = 6L, ...) {
  out <- list(
    H2 = H2(object, ...), 
    H2_overall = H2_overall(object, top_m = Inf, plot = FALSE, ...), 
    H2_pairwise = H2_pairwise(object, top_m = Inf, plot = FALSE, ...), 
    H2_threeway = H2_threeway(object, top_m = Inf, plot = FALSE, ...)
  )
  out <- out[sapply(out, Negate(is.null))]
  
  addon <- "(only for features with strong overall interactions)"
  txt <- c(
    H2 = "Proportion of prediction variability unexplained by main effects of v",
    H2_overall = "Strongest overall interactions", 
    H2_pairwise = paste0("Strongest relative pairwise interactions\n", addon),
    H2_threeway = paste0("Strongest relative three-way interactions\n", addon)
  )
  
  for (nm in names(out)) {
    cat(txt[[nm]])
    cat("\n")
    print(out[[nm]])
    cat("\n")
  }
  invisible(out)
}

#' Plot Method for "interact" Object
#' 
#' Plot method for object of class "interact".
#'
#' @param x An object of class "interact".
#' @param which Which statistic(s) to be shown? Default is `1:2`, i.e., show both
#'   \eqn{H^2_j} (1) and \eqn{H^2_{jk}} (2). To also show three-way interactions,
#'   use `1:3`.
#' @param top_m Maximum number of rows of results to plot.
#' @param fill Color of bars (univariate case).
#' @param facet_scales Value passed to `ggplot2::facet_wrap(scales = ...)`.
#' @param ncol Passed to `ggplot2::facet_wrap()`.
#' @param ... Further arguments passed to statistics, e.g., `normalize = FALSE`.
#' @returns An object of class "ggplot".
#' @export
#' @seealso See [interact()] for examples.
plot.interact <- function(x, which = 1:2, top_m = 15L, fill = "#2b51a1", 
                          facet_scales = "free", ncol = 2L, ...) {
  ids <- c("Overall", "Pairwise", "Threeway")
  funs <- c(H2_overall, H2_pairwise, H2_threeway)
  dat <- list()
  i <- 1L
  for (f in funs) {
    if (i %in% which)
      dat[[i]] <- mat2df(f(x, top_m = top_m, plot = FALSE, ...), id = ids[i])
    i <- i + 1L
  }
  dat <- do.call(rbind, dat)
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = value_, y = variable_)) +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab("Value")
  
  if (length(unique(dat[["id_"]])) > 1L) {
    p <- p + 
      ggplot2::facet_wrap(~ id_, ncol = ncol, scales = facet_scales)
  }
  if (length(unique(dat[["varying_"]])) == 1L) {
    p + ggplot2::geom_bar(fill = fill, stat = "identity")
  } else {
    p + 
      ggplot2::geom_bar(
        ggplot2::aes(fill = varying_), stat = "identity", position = "dodge"
      ) + 
      ggplot2::labs(fill = "Dim")
  }
}
