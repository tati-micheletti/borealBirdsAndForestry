# beta0L + # We removed the intercept from the local scale
# We removed age, which is accounted in the landcover
# Cluster ID needs to be sequential and is gonna be used as an index
# as.integer(factor(Data$Cluster))
# Data$CYear <- scale(Data$Year) # --> but keep track of what year the 
# we removed the intercept and the coefficient for density: beta0N + betaN[1]*
# Because the density + offset becomes the intercept 
# And we have two random effects that will modify the intercept
# We could also have betaL[2] and betaN[1] being drawn from the same hyperparameter 
# if we think that we can have similar effects on N and L
# We will check: mixing (betaL[1] and beta[1])

# If subsetting, do it by cluster

# Factorial of this TODO
# 1) as is
# 2) We might take NN out and just put one equation in the other and remove betaL[1]
# 3) Multivariate normal for beta: b ~ mvnorm(mu.beta, omega.beta) where omega.beta is a matrix? Check
