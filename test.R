fit <- lm(as.matrix(iris[1:2]) ~ Petal.Length + Petal.Width * Species, data = iris)
v <- c("Petal.Length", "Petal.Width", "Species")
inter <- i_compute(fit, v = v, X = iris[c(1:150, 1:50), ], verbose = FALSE)
inter$H2_j
# Sepal.Length Sepal.Width
# Petal.Length  0.000000000  0.00000000
# Petal.Width   0.041065008  0.01206301
# Species       0.007672614  0.06456307

head(inter$F_jk$`Petal.Width:Species`)
# Sepal.Length Sepal.Width
# [1,]    0.9710553   0.4797139
# [2,]    0.9710553   0.4797139
# [3,]    0.9710553   0.4797139
# [4,]    0.9710553   0.4797139
# [5,]    0.9710553   0.4797139
# [6,]    1.0543789   0.6321243

inter <- i_compute(fit, v = v, X = iris, verbose = FALSE,
w = c(rep(2, times = 50), rep(1, times = 100)))
inter$H2_j  # Relative interaction strength (H^2)
# Sepal.Length Sepal.Width
# Petal.Length  0.000000000  0.00000000
# Petal.Width   0.041065008  0.01206301
# Species       0.007672614  0.06456307

head(inter$F_jk$`Petal.Width:Species`)
# Sepal.Length Sepal.Width
# [1,]    0.9710553   0.4797139
# [2,]    0.9710553   0.4797139
# [3,]    0.9710553   0.4797139
# [4,]    0.9710553   0.4797139
# [5,]    0.9710553   0.4797139
# [6,]    1.0543789   0.6321243