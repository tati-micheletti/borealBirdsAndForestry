data(trees, package="datasets")
trees <- trees[, c(2,3)]
names(trees) <- c("x", "times")

microbenchmark::microbenchmark(lm(x ~ times, data = trees,  na.action = na.omit), 
  RcppArmadillo::fastLmPure(X = cbind(1, trees$times), y = trees$x))

# Extract values:
coef1 <- coef(summary(mod1))["times","Estimate"]
sig1 <- coef(summary(mod1))["times","Pr(>|t|)"]

coef2 <- mod2$coefficients[2]
pVal2 <- 2*pt(abs(mod2$coefficients/mod2$stderr), mod2$df.residual, lower.tail=FALSE)[2]
