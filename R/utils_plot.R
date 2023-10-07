#' Rotate x labels in plots
#' 
#' @noRd
#' @keywords internal
#' 
#' @returns A theme object.
rotate_x_labs <- function() {
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
  )
}

#' Color Scale based on column
#' 
#' @noRd
#' @keywords internal
#' @param x A vector/factor.
#' 
#' @returns Discrete or continuous viridis color scale
get_color_scale <- function(x) {
  disc <- is.factor(x) || is.character(x) || is.logical(x)
  if (disc) ggplot2::scale_color_viridis_d else ggplot2::scale_color_viridis_c
}

#' Revert Levels
#' 
#' Horizontal barplots use unintuitive sortings. This function reverts
#' the factor levels of a data.frame returned by `mat2df()`.
#' 
#' @noRd
#' @keywords internal
#' @param df A data.frame.
#' 
#' @returns A data.frame with reverted factor levels.
barplot_reverter <- function(df) {
  transform(
    df, 
    variable_ = factor(variable_, levels = rev(levels(variable_))),
    varying_ = factor(varying_, levels = rev(levels(varying_)))
  )
}

#' Stack some Columns
#' 
#' Internal function used in the plot method for "pd" objects. The function brings
#' wide columns `to_stack` (the prediction dimensions) into long form.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param data A data.frame.
#' @param to_stack Column names in `data` to bring from wide to long form.
#' @returns 
#'   A data.frame with variables not in `to_stack`, a column "varying_" with
#'   the column name from `to_stack`, and finally a column "value_" with stacked values.
poor_man_stack <- function(data, to_stack) {
  if (!is.data.frame(data)) {
    stop("'data' must be a data.frame.")
  }
  keep <- setdiff(colnames(data), to_stack)
  out <- lapply(
    to_stack, 
    FUN = function(z) cbind.data.frame(data[keep], varying_ = z, value_ = data[, z])
  )
  out <- do.call(rbind, out)
  transform(out, varying_ = factor(varying_, levels = to_stack))
}

#' Matrix to DF
#' 
#' Internal function used in the plot method for "interact" objects. It turns
#' matrix objects into (long) data.frames.
#' 
#' @noRd
#' @keywords internal
#' 
#' @param mat A matrix.
#' @param id Value of column to be added as "id_".
#' @returns A data.frame, or `NULL`.
mat2df <- function(mat, id = "Overall") {
  if (is.null(mat)) {
    return(NULL)
  }
  if (!is.matrix(mat)) {
    stop("'mat' must be a matrix.")
  }
  pred_names <- colnames(mat)
  K <- ncol(mat)
  nm <- rownames(mat)
  if (is.null(pred_names)) {
    pred_names <- if (K == 1L) "y" else paste0("y", seq_len(K))
    colnames(mat) <- pred_names
  }
  if (is.null(nm)) {
    nm <- seq_along(nrow(mat))
  }
  out <- cbind.data.frame(id_ = id, variable_ = factor(nm, levels = nm), mat)
  poor_man_stack(out, to_stack = pred_names)
}
