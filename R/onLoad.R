.onLoad <- function(libname, pkgname) {
  op <- options()
  op.hstats <- list(
    hstats.fill = "#fca50a",
    hstats.color = "#420a68",
    hstats.viridis_args = list(begin = 0.2, end = 0.8, option = "B")
  )
  toset <- !(names(op.hstats) %in% names(op))
  if (any(toset)) {
    options(op.hstats[toset])
  }
  invisible()
}
