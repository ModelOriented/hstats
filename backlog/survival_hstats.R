library(ranger)
library(survival)
library(hstats)
library(ggplot2)

set.seed(1)

fit <- ranger(Surv(time, status) ~ ., data = veteran)
fit2 <- ranger(time ~ . - status, data = veteran)
fit3 <- ranger(time ~ . - status, data = veteran, quantreg = TRUE)
fit4 <- ranger(status ~ . - time, data = veteran, probability = TRUE)

xvars <- setdiff(colnames(veteran), c("time", "status"))

hstats(fit, X = veteran, v = xvars[1:2], survival = "prob")
hstats(fit, X = veteran, v = xvars[1:2], survival = "chf")
hstats(fit2, X = veteran, v = xvars[1:2])
hstats(fit3, X = veteran, v = xvars[1:2], type = "quantiles")
hstats(fit4, X = veteran, v = xvars[1:2])

partial_dep(fit, X = veteran, v = "celltype")
partial_dep(fit, X = veteran, v = "celltype", survival = "prob")
partial_dep(fit2, X = veteran, v = "celltype")
partial_dep(fit3, X = veteran, v = "celltype", type = "quantiles")
partial_dep(fit4, X = veteran, v = "celltype")


ice(fit, X = veteran, v = "celltype")
ice(fit, X = veteran, v = "celltype", survival = "prob")
ice(fit2, X = veteran, v = "celltype")
ice(fit3, X = veteran, v = "celltype", type = "quantiles")
ice(fit4, X = veteran, v = "celltype")

