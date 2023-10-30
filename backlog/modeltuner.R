library(hstats)
library(modeltuner)

form <- Sepal.Length ~ Sepal.Width + Petal.Length + Species
fit_lm <- model(lm(form, iris, weights = Petal.Width))
fit_glm <- model(glm(form, iris, weights = Petal.Width, family = Gamma(link = "log")))

mm <- c(lm = fit_lm, glm = fit_glm)
predict(mm, head(iris))

average_loss(mm, X = iris, y = "Sepal.Length", BY = "Species", w = "Petal.Width") |> 
  plot()
pd <- partial_dep(mm, v = "Sepal.Width", X = iris, BY = "Species", w = "Petal.Width") 
plot(pd, show_points = FALSE)
plot(pd, show_points = FALSE, swap_dim = TRUE)
ice(mm, v = "Sepal.Width", X = iris, BY = "Species") |> 
  plot(center = TRUE)

perm_importance(mm, X = iris, y = "Sepal.Length", w = "Petal.Width") |> 
  plot()

# Interaction statistics (H-statistics)
(H <- hstats(mm, X = iris[-1], w = "Petal.Width"))
plot(H)
h2_pairwise(H, normalize = FALSE, squared = FALSE) |> 
  plot(swap_dim = TRUE)

