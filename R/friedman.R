# set.seed(1)
# 
# n <- 500
# X <- matrix(runif(10 * n), nrow = n)
# df <- data.frame(X)
# df <- transform(
#   df, 
#   y = 10 * sin(pi * X1 * X2) + 20 * (X3 - 0.5)^2 + 10 * X4 + 5 * X5 + rnorm(n)
# )
# cor(df$y, X)
# 
# 
# library(ranger)
# 
# fit <- ranger(y ~ ., data = df)
# fit
# 
# h <- fx_friedmans_h(fit, v = colnames(df)[1:10], X = df, pairwise = TRUE, normalize = FALSE)
# h[order(-h$Stat), ]
