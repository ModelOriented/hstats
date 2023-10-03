.onLoad <- function(libname, pkgname) {
  op <- options()
  op.hstats <- list(
    hstats.fill = "#fca50a",
    hstats.color = "#3b528b",
    hstats.scale_fill_d = ggplot2::scale_fill_viridis_d(
      begin = 0.25, end = 0.85, option = "inferno"
    )
  )
  toset <- !(names(op.hstats) %in% names(op))
  if (any(toset)) {
    options(op.hstats[toset])
  }
  invisible()
}