# model {
#   
#   #N is density, i is site, j is year of survey
#   
#   # Priors
#   ## Suitability and abundance
#   phi ~ dbern(0.5) # proportion of suitable habitat
#   logit.phi <- logit(phi)
#   
#   ## Random effects
#   tau.cluster <- pow(sd.cluster, -2)
#   sd.cluster ~ dunif(0, 2) # year heterogeneity in lambda
#   tau.year <- pow(sd.year, -2)
#   sd.year ~ dunif(0, 2) # cluster heterogeneity in lambda
#   
#   
#   ## Parameters
#   beta0N ~ dnorm(0, 0.1)
#   beta0L ~ dnorm(0, 0.1)
#   alpha0 ~ dnorm(0, 0.1)
#   for (k in 1:3) {  # regression params in lambda N
#     betaN[k] ~ dnorm(0, 1)}
#   for (k in 1:2) {  # regression params in lambda L
#     betaL[k] ~ dnorm(0, 1)}
#   for (k in 1:5) {  # regression params in lambda L
#     alpha[k] ~ dnorm(0, 1)}
#   
#   # Neighborhood scale
#   ## State process
#   for (i in 1:siteData){
#     omega[i] ~ dbern(phi) # ZI part (?suitability? of the site). Phi = 0, habitat is not suitable; phi = 1, is suitable
#     Cluster[i] ~ dnorm(0, tau.cluster) # random cluster effects in log(density)
#     Year ~ dnorm(0, tau.year) # random year effects in logit(probability of observation)
#     NN[i]  ~ dpois(mu.poisson[i])
#     lambda1[i] <- exp(log.lambda1[i])
#     log.lambda1[i] <- beta0N + inprod(betaN[ ], N.neig[i, ]) + offset + Cluster[i] * switchCluster + Year * switchYear
#     # loglam.lim[i] <- min(250, max(-250, log.lambda1[i])) # 'Stabilize' log
#     mu.poisson[i] <- omega[i] * lambda1[i]
#     
#     # Local scale
#     ## State process
#     NL[i]  ~ dpois(lambda2[i])
#     lambda2[i] <- exp(log.lambda2[i])
#     log.lambda2[i] <- beta0L + inprod(betaN[ ], N.loc[i, ]) + NN[i]
#     
#   } # end i
#   
#     ## Observation process
#   for (s in 1:siteCounts){
#     for (j in 1:nYears){
#       counts[s, j] ~ dbin(NL[s], p[s, j])
#       p[s,j] <- 1 / (1 + exp(-lp.lim[s,j]))
#       lp.lim[s,j] <- min(250, max(-250, lp[s,j])) # 'Stabilize' logit
#       lp[s,j] <- alpha0 + inprod(alpha[ ], N.locObs[s, ])
#       
#     } # end j
# } # end s
#       
#       #~~~~~~~~~~~ FROM HERE ON STILL TO DEVELOP ~~~~~~~~#
#       
#       # Posterior predictive distributions of Chi2 discrepancy
#       for (i in 1:nsite) {
#         for (j in 1:nrep) {
#           y.sim[i,j] ~ dbin(p[i,j], N[i]) # Create new data set under model
#           e.count[i,j] <- N[i] * p[i,j] # Expected datum
#           # Chi-square discrepancy for the actual data
#           chi2.actual[i,j] <- pow((y[i,j]-e.count[i,j]),2) / (e.count[i,j]+e)
#           # Chi-square discrepancy for the simulated ('perfect') data
#           chi2.sim[i,j] <- pow((y.sim[i,j]-e.count[i,j]),2) / (e.count[i,j]+e)
#           # Add small value e to denominator to avoid division by zero
#         }
#       }
#       # Add up individual Chi2 values for overall fit statistic
#       fit.actual <- sum(chi2.actual[,]) # Fit statistic for actual data set
#       fit.sim <- sum(chi2.sim[,]) # Fit statistic for a fitting model
#       bpv <- step(fit.sim-fit.actual) # Bayesian p-value
#       c.hat <- fit.actual/fit.sim # c-hat estimate
#       # Derived parameters: Total abundance at all sampled sites # NOT SURE. NEEDS TO BE FIXED, AS EVERYTHING IS ABOUT DENSITY, NOT ABUNDANCE!
#       Ntotal <- sum(N[])
#   
# } # end of model
# 
