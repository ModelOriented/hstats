library(modeltuner)
library(hstats)

fit_lm <- model(lm(Sepal.Length ~ ., iris))
fit_glm <- model(glm(Sepal.Length ~ ., iris, family = Gamma(link = "log")))

mm <- c(lm = fit_lm, glm = fit_glm)
predict(mm, head(iris))

partial_dep(mm, v = "Sepal.Width", X = iris, BY = "Species") |> 
  plot(facet_scales = "fixed", show_points = FALSE)
ice(mm, v = "Sepal.Width", X = iris, BY = "Species") |> 
  plot(facet_scales = "fixed")

v <- colnames(iris[-1])
perm_importance(mm, v = v, X = iris, y = iris[, 1]) |> 
  plot()

# Interaction statistics (H-statistics)
H <- hstats(mm, v = v, X = iris)
H
plot(H)
h2_pairwise(H, normalize = FALSE, squared = FALSE, plot = TRUE)
