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
#' @param top_m How many rows should be plotted? (`Inf` for all.)
#' @param multi_output How should multi-output models be represented? 
#'   Either as "grouped" barplot (the default) or via "facets".
#' @param fill Color of bars.
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
                               multi_output = c("grouped", "facets"),
                               fill = "#2b51a1", facet_scales = "free", 
                               ncol = 2L, rotate_x = FALSE,
                               err_type = c("SE", "SD", "No"), ...) {
  err_type <- match.arg(err_type)
  multi_output <- match.arg(multi_output)
  
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
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = value_, y = variable_)) + 
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(x[["description"]])
  
  K <- ncol(M)
  grouped <- multi_output == "grouped" && K > 1L
  if (!grouped) {
    p <- p + ggplot2::geom_bar(fill = fill, stat = "identity", ...)
  } else {
    p <- p + ggplot2::geom_bar(
      ggplot2::aes(fill = varying_), stat = "identity", position = "dodge", ...
    ) + 
      ggplot2::labs(fill = "Response")
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
      ) + ggplot2::theme(legend.title = ggplot2::element_blank())
    }
  }
  if (K > 1L && multi_output == "facets") {
    p <- p + ggplot2::facet_wrap("varying_", ncol = ncol, scales = facet_scales)
  }
  if (rotate_x) {
    p + rotate_x_labs()
  }
  p
}
