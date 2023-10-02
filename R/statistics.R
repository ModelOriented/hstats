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
#' Internal helper function that provides an object of class "hstat_matrix".
#' 
#' @noRd
#' @keywords internal
#' @param statistic Name of statistic as stored in "hstats" object.
#' @inheritParams h2_overall
#' @returns A character string.
get_hstat_matrix <- function(statistic, object, normalize = TRUE, squared = TRUE, 
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
      normalize = normalize, 
      squared = squared, 
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
get_description <- function(statistic, normalize = TRUE, squared = TRUE) {
  stat_names <- c(
    h2 = "H",
    h2_overall = "Overall H", 
    h2_pairwise = "Pairwise H", 
    h2_threeway = "Three-way H",
    pd_importance = "PDI"
  )
  paste0(
    stat_names[statistic], 
    get_description_details(normalized = normalized, squared = squared)
  )
}

#' Description Text Details
#' 
#' Internal helper function that provides the details of description text.
#' 
#' @noRd
#' @keywords internal
#' @param normalize Flag.
#' @param squared Flag.
#' @returns A character string.
get_description_details <- function(normalize = TRUE, squared = TRUE) {
  paste0(
    if (squared) "-squared" else "", 
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
#' @param x An object of class "hstats_matrix".
#' @inheritParams plot.hstats
#' @param ... Arguments passed to [ggplot2::geom_bar].
#' @export
#' @returns An object of class "ggplot".
#' @seealso See [perm_importance()] for examples.
plot.hstats_matrix <- function(x, top_m = 15L, fill = "#2b51a1", ...) {
  M <- x[["M"]]
  if (is.null(M)) {
    message("Nothing to plot!")
    return(NULL)
  }
  M <- utils::head(M, top_m)
  p <- ggplot2::ggplot(mat2df(M), ggplot2::aes(x = value_, y = variable_)) +
    ggplot2::ylab(ggplot2::element_blank()) +
    ggplot2::xlab(x[["description"]])
  
  if (ncol(M) == 1L) {
    p + ggplot2::geom_bar(fill = fill, stat = "identity", ...)
  } else {
    p + 
      ggplot2::geom_bar(
        ggplot2::aes(fill = varying_), stat = "identity", position = "dodge", ...
      ) + 
      ggplot2::labs(fill = "Response")
  }
}
