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

#' Postprocessing of Statistics
#' 
#' Function to apply typical postprocessing steps to a Friedman-Popescu type statistic.
#' 
#' @noRd
#' @keywords internal
#' 
#' @inheritParams H2_overall
#' @param num Matrix or vector of statistic.
#' @param denom Denominator of statistic (a matrix, number, or vector compatible with `num`).
#' @returns Matrix or vector of statistics. If length of output is 0, then `NULL`.
postprocess <- function(num, denom = 1, normalize = TRUE, squared = TRUE, 
                        sort = TRUE, zero = TRUE) {
  out <- num
  if (normalize) {
    if (length(denom) == 1L || length(num) == length(denom)) {
      out <- out / denom
    } else if (length(denom) == ncol(num)) {
      out <- sweep(out, MARGIN = 2L, STATS = denom, FUN = "/")
    } else {
      stop("Normalization error")
    }
  }
  if (!squared) {
    out <- sqrt(out)
  }
  if (sort) {
    if (is.matrix(out)) {
      out <- out[order(-rowSums(out)), , drop = FALSE]
    } else {
      out <- sort(out, decreasing = TRUE)
    }
  }
  if (!zero) {
    if (is.matrix(out)) {
      out <- out[rowSums(out) > 0, , drop = FALSE]
    } else {
      out <- out[out > 0]
    }
  }
  # out <- utils::head(out, n = top_m)
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

