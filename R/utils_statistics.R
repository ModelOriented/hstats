#' Zap Small Values
#' 
#' Internal function. Sets very small or non-finite (NA, ...) values in vector, 
#' matrix or data.frame to 0.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param x Vector, matrix, or data.frame.
#' @param eps Threshold, below which absolute values are set to 0.
#' @returns Same as `x` but with values below `eps` replaced by 0.
.zap_small <- function(x, eps = 1e-8) {
  zero <- abs(x) < eps | !is.finite(x)
  if (any(zero)) {
    x[zero] <- 0
  }
  x
}

#' Initializer of Numerator Statistics
#' 
#' Internal helper function that returns a matrix of all zeros with the right
#' column and row names for statistics of any "way". If some features have been dropped
#' from the statistics calculations, they are added as 0.
#' 
#' @noRd
#' @keywords internal
#' @param x A list containing the elements "v", "K", "pred_names", "v_pairwise", 
#'   "v_threeway", "pairwise_m", "threeway_m".
#' @param way Integer between 1 and 3 of the order of the interaction.
#' @returns A matrix of all zeros.
init_numerator <- function(x, way = 1L) {
  stopifnot(way %in% 1:3)
  
  v <- x[["v"]]
  K <- x[["K"]]
  pred_names <- x[["pred_names"]]
  
  # Simple case
  if (way == 1L) {
    return(matrix(nrow = length(v), ncol = K, dimnames = list(v, pred_names)))
  }
  
  # Determine v_cand_0, which is v_cand with additional features to end up with length m
  if (way == 2L) {
    v_cand <- x[["v_pairwise"]]
    m <- x[["pairwise_m"]]
  } else {
    v_cand <- x[["v_threeway"]]
    m <- x[["threeway_m"]]
  }
  m_miss <- m - length(v_cand)
  if (m_miss > 0L) {
    v_cand_0 <- c(v_cand, utils::head(setdiff(v, v_cand), m_miss))
    v_cand_0 <- v[v %in% v_cand_0]  # Bring into order of v
  } else {
    v_cand_0 <- v_cand
  }
  
  # Get all interactions of order "way". c() turns the array into a vector
  cn0 <- c(utils::combn(v_cand_0, m = way, FUN = paste, collapse = ":"))
  matrix(0, nrow = length(cn0), ncol = K, dimnames = list(cn0, pred_names))
}

#' Postprocessing of Statistics
#' 
#' Function to apply typical postprocessing steps to a Friedman-Popescu type statistic.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams H2_overall
#' @param num Matrix with numerator statistics.
#' @param denom Vector or matrix with denominator statistics.
#' @returns Matrix of statistics, or `NULL`.
postprocess <- function(num, denom = rep(1, times = NCOL(num)), 
                        normalize = TRUE, squared = TRUE, 
                        sort = TRUE, zero = TRUE) {
  stopifnot(
    is.matrix(num),
    is.matrix(denom) || is.vector(denom),  # already covered by the next condition
    is.matrix(denom) && all(dim(num) == dim(denom)) ||  ## h2_pairwise/threeway
      is.vector(denom) && length(denom) == ncol(num)    ## all other stats
  )
  if (normalize) {
    if (is.matrix(denom)) {
      out <- num / denom
    } else {
      out <- sweep(num, MARGIN = 2L, STATS = denom, FUN = "/")
    }
  } else {
    out <- num
  }
  if (!squared) {
    out <- sqrt(out)
  }
  if (sort) {
    out <- out[order(-rowSums(out)), , drop = FALSE]
  }
  if (!zero) {
    out <- out[rowSums(out != 0) > 0, , drop = FALSE]
  }
  if (length(out) == 0L) NULL else out
}

#' Basic Statistic Function
#' 
#' Internal helper function that provides an object of class "hstats_matrix".
#' 
#' @noRd
#' @keywords internal
#' @param statistic Name of statistic as stored in "hstats" object.
#' @inheritParams h2_overall
#' @returns A character string.
get_hstats_matrix <- function(statistic, object, normalize = TRUE, squared = TRUE, 
                              sort = TRUE, zero = TRUE, ...) {
  s <- object[[statistic]]
  if (!is.null(s)) {
    M <- postprocess(
      num = s$num,
      denom = s$denom,
      normalize = normalize, 
      squared = squared, 
      sort = sort,
      zero = zero
    )
  } else {
    M <- NULL
  }
  
  structure(
    list(
      M = M,
      SE = NULL,
      mrep = NULL,
      statistic = statistic,
      description = get_description(statistic, normalize = normalize, squared = squared)
    ), 
    class = "hstats_matrix"
  )
}

#' Description Text
#' 
#' Internal helper function that provides the description text of a statistic.
#' 
#' @noRd
#' @keywords internal
#' @param statistic Name of statistic.
#' @param normalize Flag.
#' @param squared Flag.
#' @returns A character string.
get_description <- function(statistic, normalize = TRUE, squared = FALSE) {
  stat_names <- c(
    h2 = "H",
    h2_overall = "Overall H", 
    h2_pairwise = "Pairwise H", 
    h2_threeway = "Three-way H",
    pd_importance = "PDI"
  )
  paste0(
    stat_names[statistic], 
    if (squared) "^2" else "", 
    if (normalize) " (normalized)" else " (unnormalized)"
  )
}

# Methods

#' Prints "hstats_matrix" Object
#' 
#' Print method for object of class "hstats_matrix".
#'
#' @param x An object of class "hstats_matrix".
#' @param top_m Number of rows to print.
#' @param ... Currently not used.
#' @returns Invisibly, the input is returned.
#' @export
print.hstats_matrix <- function(x, top_m = Inf, ...) {
  cat(x[["description"]])
  cat("\n")
  M <- utils::head(x[["M"]], top_m)
  if (!(x[["statistic"]] %in% c("h2_pairwise", "h2_threeway")) || length(M) == 1L) {
    M <- drop(M)
  }
  print(M)
  invisible(x)
}

#' Dimensions of "hstats_matrix" Object
#' 
#' Implies `nrow()` and `ncol()`.
#'
#' @param x An object of class "hstats_matrix".
#' @returns
#'   A numeric vector of length two providing the number of rows and columns
#'   of "M" object stored in `x`.
#' @examples
#' fit <- lm(Sepal.Length ~ . + Petal.Width:Species, data = iris)
#' s <- hstats(fit, X = iris[-1])
#' x <- h2_pairwise(s)
#' dim(x)
#' nrow(x)
#' ncol(x)
#' @export
dim.hstats_matrix <- function(x) {
  dim(x[["M"]])
}

#' Dimnames of "hstats_matrix" Object
#'
#' Extracts dimnames of the "M" matrix in `x`. Implies `rownames()` and `colnames()`.
#'
#' @param x An object of class "hstats_matrix".
#' @returns Dimnames of the statistics matrix.
#' @examples
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' s <- hstats(fit, X = iris[3:5], verbose = FALSE)
#' x <- h2_pairwise(s)
#' dimnames(x)
#' rownames(x)
#' colnames(x)
#' @export
dimnames.hstats_matrix <- function(x) {
  dimnames(x[["M"]])
}

#' Dimnames (Replacement Method) of "hstats_matrix"
#'
#' This implies `colnames(x) <- ...`.
#'
#' @param x An object of class "hstats_matrix".
#' @param value A list with rownames and column names compliant with `$M` (and `$SE`).
#' @returns Like `x`, but with replaced dimnames.
#' @examples
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' s <- hstats(fit, X = iris[3:5], verbose = FALSE)
#' x <- h2_overall(s)
#' colnames(x) <- c("Sepal Length", "Sepal Width")
#' plot(x)
#' 
#' rownames(x)[2:3] <- c("Petal Width", "Petal Length")
#' plot(x)
#' @export
`dimnames<-.hstats_matrix` <- function(x, value) {
  dimnames(x[["M"]]) <- value
  if (!is.null(x[["SE"]])) {
    dimnames(x[["SE"]]) <- value
  }
  x
}

#' Subsets "hstats_matrix" Object
#'
#' Use standard square bracket subsetting to select rows and/or columns of
#' statistics "M" (and "SE" in case of permutation importance statistics).
#' Implies `head()` and `tail()`.
#'
#' @param x An object of class "hstats_matrix".
#' @param i Row subsetting.
#' @param j Column subsetting.
#' @param ... Currently unused.
#' @returns A new object of class "hstats_matrix".
#' @examples
#' fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
#' imp <- perm_importance(fit, X = iris, y = c("Sepal.Length", "Sepal.Width"))
#' head(imp, 1)
#' tail(imp, 2)
#' imp[1, "Sepal.Length"]
#' imp[1]
#' imp[, "Sepal.Width"]$SE
#' plot(imp[, "Sepal.Width"])
#' @export
`[.hstats_matrix` <- function(x, i, j, ...) {
  x$M <- x$M[i, j, drop = FALSE]
  if (!is.null(x$SE)) {
    x$SE <- x$SE[i, j, drop = FALSE]
  }
  x
}

#' Plots "hstats_matrix" Object
#'
#' Plot method for objects of class "hstats_matrix".
#'
#' @importFrom ggplot2 .data
#' @param x An object of class "hstats_matrix".
#' @param top_m How many rows should be plotted? `Inf` for all.
#' @param fill Fill color of ungrouped bars. The default equals the global option
#'   `hstats.fill = "#fca50a"`. To change the global option, use
#'   `options(stats.fill = new value)`.
#' @param swap_dim Switches the role of grouping and facetting (default is `FALSE`).
#' @param viridis_args List of viridis color scale arguments, see
#'   `[ggplot2::scale_color_viridis_d()]`. 
#'   The default points to the global option `hstats.viridis_args`, 
#'   which corresponds to `list(begin = 0.2, end = 0.8, option = "B")`.
#'   E.g., to switch to a standard viridis scale, you can change the default via 
#'   `options(hstats.viridis_args = list())`, or set `viridis_args = list()`. 
#' @param facet_scales Value passed as `scales` argument to `[ggplot2::facet_wrap()]`.
#' @param ncol Passed to `[ggplot2::facet_wrap()]`.
#' @param rotate_x Should x axis labels be rotated by 45 degrees?
#' @param err_type The error type to show, by default "SE" (standard errors). Set to
#'   "SD" for standard deviations (SE * sqrt(m_rep)), or "No" for no bars. 
#'   Currently, supported only for [perm_importance()].
#' @param ... Passed to [ggplot2::geom_bar()].
#' @export
#' @returns An object of class "ggplot".
plot.hstats_matrix <- function(x, top_m = 15L,
                               fill = getOption("hstats.fill"),
                               swap_dim = FALSE,
                               viridis_args = getOption("hstats.viridis_args"),
                               facet_scales = "fixed", 
                               ncol = 2L, rotate_x = FALSE,
                               err_type = c("SE", "SD", "No"), ...) {
  err_type <- match.arg(err_type)
  M <- x[["M"]]
  if (is.null(M)) {
    message("Nothing to plot!")
    return(NULL)
  }
  
  M <- utils::head(M, top_m)
  err <- utils::head(x[["SE"]], top_m)
  if (is.null(err)) {
    err_type <- "No"
  } else if (err_type == "SD") {
    err <- err * sqrt(x[["m_rep"]])
  }
  
  df <- mat2df(M)
  if (err_type != "No") {
    df[["error_"]] <- mat2df(err)[["value_"]]
  }
  df <- barplot_reverter(df, group = !swap_dim)
  
  if (is.null(viridis_args)) {
    viridis_args <- list()
  }
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = value_, y = variable_)) + 
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(x[["description"]])
  
  K <- ncol(M)
  grouped <- K > 1L && !swap_dim 
  if (!grouped) {
    p <- p + ggplot2::geom_bar(fill = fill, stat = "identity", ...)
  } else {
    p <- p + ggplot2::geom_bar(
      ggplot2::aes(fill = varying_), stat = "identity", position = "dodge", ...
    ) + 
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      do.call(ggplot2::scale_fill_viridis_d, viridis_args) +
      ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))
  }
  if (err_type != "No") {
    if (!grouped) {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(xmin = value_ - error_, xmax = value_ + error_), 
        width = 0, 
        color = "black"
      )
    } else {
      p <- p + ggplot2::geom_errorbar(
        ggplot2::aes(xmin = value_ - error_, xmax = value_ + error_, group = varying_), 
        width = 0, 
        color = "black",
        position = ggplot2::position_dodge(0.9)
      )
    }
  }
  if (K > 1L && swap_dim) {
    p <- p + ggplot2::facet_wrap("varying_", ncol = ncol, scales = facet_scales)
  }
  if (nrow(M) == 1L) {
    p <- p + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }
  if (rotate_x) {
    p <- p + rotate_x_labs()
  }
  p
}

