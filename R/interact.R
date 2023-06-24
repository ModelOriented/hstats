#' Calculate Interaction Statistics
#' 
#' @description
#' This is the main function of the package. It does the expensive calculations behind
#' interaction statistics:
#' - Total interaction strength \eqn{H^2}, a statistic measuring the proportion
#'   of prediction variability unexplained by main effects, see [H2()] for the definition.
#' - Friedman and Popescu's \eqn{H^2_j} statistic of overall interaction strength per
#'   feature, see [H2_j()] for its definition.
#' - Friedman and Popescu's \eqn{H^2_{jk}} statistic of pairwise interaction strength,
#'   see [H2_jk()] for details.
#'  
#'  Instead of getting these statistics via `summary()`, you can obtain them via the 
#'  more flexible functions [H2()], [H2_j()], and [H2_jk()].
#'  
#' @inheritParams partial_dep
#' @param pairwise_m Number of features for which pairwise statistics are calculated.
#'   The features are selected based on Friedman and Popescu's overall interaction 
#'   strength \eqn{H^2_j} (rowwise maximum in the multivariate case). 
#'   Set to `length(v)` to calculate every pair and to 0 to avoid pairwise calculations. 
#' @param verbose Should a progress bar be shown? The default is `TRUE`.
#' @returns 
#'   An object of class "interact" containing these elements:
#'   - `f`: Matrix with (centered) predictions \eqn{F}.
#'   - `mean_f2`: (Weighted) column means of `f`. Used to normalize most statistics.
#'   - `F_j`: List of matrices, each representing (centered) 
#'     partial dependence functions \eqn{F_j}.
#'   - `F_not_j`: List of matrices with (centered) partial dependence 
#'     functions \eqn{F_{\setminus j}} of other features.
#'   - `F_jk`: List of matrices, each representing (centered) bivariate 
#'     partial dependence functions \eqn{F_{jk}}.
#'   - `w`: Same as input `w`.
#'   - `v`: Same as input `v`.
#'   - `v_pairwise`: Subset of `v` with largest `H2_j` used for pairwise calculations.
#'   - `combs`: Named list of variable pairs for which pairwise partial 
#'     dependence functions are available.
#' @references
#'   Friedman, Jerome H., and Bogdan E. Popescu. *"Predictive Learning via Rule Ensembles."*
#'     The Annals of Applied Statistics 2, no. 3 (2008): 916-54.
#' @export
#' @seealso [H2()], [H2_j()], and [H2_jk()] for specific statistics calculated from the
#'   resulting object.
#' @examples
#' # MODEL ONE: Linear regression
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' inter
#' summary(inter)
#' 
#' # MODEL TWO: Multi-response linear regression
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' v <- c("Petal.Length", "Petal.Width", "Species")
#' inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#' summary(inter)
#'
#' # MODEL THREE: Gamma GLM with log link
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
#' summary(inter)
#' 
interact <- function(object, ...) {
  UseMethod("interact")
}

#' @describeIn interact Default interact method.
#' @export
interact.default <- function(object, v, X, pred_fun = stats::predict, pairwise_m = 5L, 
                             n_max = 300L, w = NULL, verbose = TRUE, ...) {
  basic_check(X = X, v = v, pred_fun = pred_fun, w = w)
  
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
  mean_f2 <- wcolMeans(f^2, w = w)
  
  # Initialize first progress bar
  p <- length(v)
  show_bar <- verbose && p >= 2L
  if (show_bar) {
    cat("Univariable calculations...\n")
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
  
  # Pairwise stats are calculated only for subset of features with large interactions
  h2_j <- H2_j_raw(
    F_j = F_j, 
    F_not_j = F_not_j, 
    f = f, 
    mean_f2 = mean_f2, 
    w = w, 
    normalize = TRUE, 
    sort = FALSE
  )
  rowwise_max <- apply(h2_j, MARGIN = 1L, FUN = max)
  rowwise_max <- rowwise_max[rowwise_max > 0]
  if (min(pairwise_m, length(rowwise_max)) >= 2L) {
    v_pairwise <- v[v %in% names(utils::head(sort(-rowwise_max), pairwise_m))]
    combs <- utils::combn(v_pairwise, 2L, simplify = FALSE)
    n_combs <- length(combs)
    F_jk <- vector("list", length = n_combs)
    names(F_jk) <- names(combs) <- sapply(combs, paste, collapse = ":")
    
    show_bar <- verbose && n_combs >= 2L
    if (show_bar) {
      cat("Pairwise calculations...\n")
      pb <- utils::txtProgressBar(1L, max = n_combs, style = 3)
    }
  
    for (i in seq_len(n_combs)) {
      z <- combs[[i]]
      F_jk[[i]] <- wcenter(
        pd_raw(object, v = z, X = X, grid = X[, z], pred_fun = pred_fun, w = w, ...),
        w = w
      )
      if (show_bar) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (show_bar) {
      cat("\n")
    }
  } else {
    F_jk <- combs <- list()
    v_pairwise <- character(0L)
  }
  structure(
    list(
      f = f,
      mean_f2 = mean_f2,
      F_j = F_j, 
      F_not_j = F_not_j, 
      F_jk = F_jk,
      w = w,
      v = v,
      v_pairwise = v_pairwise,
      combs = combs
    ), 
    class = "interact"
  )
}

#' @describeIn interact Method for "ranger" models.
#' @export
interact.ranger <- function(object, v, X,
                            pred_fun = function(m, X, ...) stats::predict(m, X, ...)$predictions,
                            pairwise_m = 5L, n_max = 300L, w = NULL, 
                            verbose = TRUE, ...) {
  interact.default(
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
}

#' @describeIn interact Method for "mlr3" models.
#' @export
interact.Learner <- function(object, v, X,
                             pred_fun = function(m, X) m$predict_newdata(X)$response,
                             pairwise_m = 5L, n_max = 300L, w = NULL, 
                             verbose = TRUE, ...) {
  interact.default(
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
}

#' Print Method
#' 
#' Print method for object of class "interact". 
#'
#' @param x An object of class "interact".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' inter 
#' @seealso [interact()]
print.interact <- function(x, ...) {
  cat("'interact' object. Run summary() to get interaction statistics.\n")
  invisible(x)
}

#' Summary Method
#' 
#' Summary method for "interact" object.
#'
#' @param object An object of class "interact".
#' @param top_m Maximum number of rows of results to print.
#' @param ... Further arguments passed from other methods.
#' @returns A list of resulting matrices.
#' @export
#' @examples
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#' summary(inter)
#' 
#' # summary() returns list of statistics
#' s <- summary(inter)
#' s$H2_j  # same as H2_j(inter)
#' 
#' @seealso [interact()]
summary.interact <- function(object, top_m = 10L, ...) {
  h2 <- H2(object)
  h2_j <- H2_j(object)
  h2_jk <- H2_jk(object)
  
  cat("Proportion of prediction variability explained by interactions\n")
  print(h2)
  cat("\n")
  
  cat("Strongest overall interactions\n")
  print(utils::head(h2_j, top_m))
  cat("\n")
  
  cat("Strongest relative pairwise interactions\n")
  cat("(only for features with strong overall interactions)\n")
  print(utils::head(h2_jk, top_m))
  cat("\n")
  invisible(list(H2 = h2, H2_j = h2_j, H2_jk = h2_jk))
}

#' Plot Interaction Statistics
#' 
#' Plot method for object of class "interact".
#'
#' @param x An object of class "interact".
#' @param stat Which statistic(s) to be shown? Default is `1:2`, i.e., show both
#'   \eqn{H^2_j} (1) and \eqn{H^2_{jk}} (2).
#' @param top_m Maximum number of rows of results to plot.
#' @param fill Color of bars (univariate case).
#' @param ... Further arguments passed to statistics [H2_j()] and [H2_jk()].
#' @returns An object of class "ggplot".
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   # MODEL ONE: Linear regression
#'   fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#'   inter <- interact(fit, v = names(iris[-1]), X = iris, verbose = FALSE)
#'   plot(inter)
#' 
#'   # MODEL TWO: Multi-response linear regression
#'   fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#'   v <- c("Petal.Length", "Petal.Width", "Species")
#'   inter <- interact(fit, v = v, X = iris, verbose = FALSE)
#'   plot(inter, stat = 1)
#' }
#' 
#' @seealso [interact()]
plot.interact <- function(x, stat = 1:2, top_m = 10L, fill = "#2b51a1", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install 'ggplot2' to use plot().")
  }
  
  h2_j <- H2_j(x, top_m = top_m, ...)
  h2_jk <- H2_jk(x, top_m = top_m, ...)
  
  data <- rbind.data.frame(
    if (1L %in% stat) mat2df(h2_j, id = "Overall"),
    if (2L %in% stat) mat2df(h2_jk, id = "Pairwise")
  )

  p <- ggplot2::ggplot(data, ggplot2::aes(x = y_value, y = variable_name)) +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab("Value of statistic")
  
  if (length(stat) == 2L) {
    p <- p + ggplot2::facet_wrap(~ id_name, scales = "free")
  }
  
  if (ncol(h2_j) == 1L) {
    p + ggplot2::geom_bar(fill = fill, stat = "identity")
  } else {
    p + 
      ggplot2::geom_bar(
        ggplot2::aes(fill = y_variable), stat = "identity", position = "dodge"
      ) + 
      ggplot2::labs(fill = "Dim")
  }
}
