mway <- function(object, v, X, pred_fun = stats::predict, w = NULL, 
                 way = 2L, verbose = TRUE, ...) {
  combs <- utils::combn(v, way, simplify = FALSE)
  
  n_combs <- length(combs)
  F_way <- vector("list", length = n_combs)
  names(F_way) <- names(combs) <- sapply(combs, paste, collapse = ":")
  
  show_bar <- verbose && (n_combs >= way)
  
  if (show_bar) {
    cat(way, "way calculations...\n", sep = "-")
    pb <- utils::txtProgressBar(1L, max = n_combs, style = 3)
  }
  
  for (i in seq_len(n_combs)) {
    z <- combs[[i]]
    F_way[[i]] <- wcenter(
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
  list(combs, F_way)
}
