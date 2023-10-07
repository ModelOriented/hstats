hstats_explainer <- function(object, X, pred_fun = stats::predict, 
                             y = NULL, loss = "squared_error", 
                             w = NULL, ...) {
  structure(
    list(
      object = object,
      X = X,
      pred_fun = function(m, x) pred_fun(m, x, ...),
      y = y,
      loss = loss,
      w = w
    ),
    class = "hstats_explainer"
  )
}

