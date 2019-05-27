#N is abundance, i is the sample (data row), k is the beta parameter

# Priors
## Suitability and abundance
phi ~ dbern(0.5) # proportion of suitable habitat

## Random effects
# tau.cluster <- pow(sd.cluster, -2)
# sd.cluster ~ dunif(0, 2) # year heterogeneity in lambda
# tau.year <- pow(sd.year, -2)
# sd.year ~ dunif(0, 2) # cluster heterogeneity in lambda

## Parameters
beta0N ~ dnorm(0, 0.1)
beta0L ~ dnorm(0, 0.1)
for (k in 1:3) {  # regression params
  betaN[k] ~ dnorm(0, 1) # AbNN, %Dist100m, (1-%Dist100m)*correctedAge
}
for (k in 1:2) {  # regression params
  betaL[k] ~ dnorm(0, 1) # %Dist500m, (1-%Dist500m)*correctedAge
}

# Neighborhood scale
for (i in 1:nvisits){ # each sample point / each row of the data table
  omega[i] ~ dbern(phi) # ZI part (‘suitability’ of the sample site). Phi = 0, habitat is not suitable; phi = 1, is suitable
  # Cluster[i] ~ dnorm(0, tau.cluster) # random cluster effects in log(density)
  # Year ~ dnorm(0, tau.year) # random year effects in log(density)
  NN[i]  ~ dpois(mu.poisson[i])
  mu.poisson[i] <- omega[i] * lambda1[i]
  lambda1[i] <- exp(log.lambda1[i])
  log.lambda1[i] <- beta0N + betaN[1]*density[i] + betaN[2]*State_P_500[i] + betaN[3]*(1-State_P_500)*correctedAge +
    offset[i] # + # Offsets
  # Cluster[i] * switchCluster + Year[i] * switchYear # Random Effects
  # loglam.lim[i] <- min(250, max(-250, log.lambda1[i])) # 'Stabilize' log
  
  # Local scale
  counts[i]  ~ dpois(lambda2[i])
  lambda2[i] <- exp(log.lambda2[i])
  log.lambda2[i] <- beta0N + NN[i] + betaL[1]*State_P_100[i] + betaN[2]*(1-State_P_100)*correctedAge
  
  #     # Posterior predictive distributions of Chi2 discrepancy
  #     for (i in 1:nsite) {
  #       for (j in 1:nrep) {
  #         y.sim[i,j] ~ dbin(p[i,j], N[i]) # Create new data set under model
  #         e.count[i,j] <- N[i] * p[i,j] # Expected datum
  #         # Chi-square discrepancy for the actual data
  #         chi2.actual[i,j] <- pow((y[i,j]-e.count[i,j]),2) / (e.count[i,j]+e)
  #         # Chi-square discrepancy for the simulated ('perfect') data
  #         chi2.sim[i,j] <- pow((y.sim[i,j]-e.count[i,j]),2) / (e.count[i,j]+e)
  #         # Add small value e to denominator to avoid division by zero
  #       }
  #     }
  
  #     # Add up individual Chi2 values for overall fit statistic
  #     fit.actual <- sum(chi2.actual[,]) # Fit statistic for actual data set
  #     fit.sim <- sum(chi2.sim[,]) # Fit statistic for a fitting model
  #     bpv <- step(fit.sim-fit.actual) # Bayesian p-value
  #     c.hat <- fit.actual/fit.sim # c-hat estimate
  
  #     # Derived parameters: Total abundance at all sampled sites
  #     Ntotal <- sum(N[])
  #     
  #   } # end j
} # end i