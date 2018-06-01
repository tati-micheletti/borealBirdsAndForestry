model {
  # Specify priors
  # abundance
  beta0 ~ dnorm(0, 0.1) # log(lambda) intercept
  for(k in 1:7){ # Regression params in lambda
    beta[k] ~ dnorm(0, 1)
  }
  # Model for unexplained variance in lambda among sites
  for (i in 1:nsite){
    tau.lam[i] <- 1/var.lam[i]
    log(var.lam[i]) <- alpha.var.lam + beta.var.lam * elev[i]
  }
  # Priors for intercept and slope of linear model for variance
  alpha.var.lam ~ dunif(-1, 1)
  beta.var.lam ~ dunif(0, 3)
  # detection
  for(j in 1:3){
    alpha0[j] <- logit(mean.p[j])
    mean.p[j] ~ dunif(0, 1) # p intercept for occasions 1-3
  }
  for(k in 1:13){ # Regression params in p
    alpha[k] ~ dnorm(0, 1)
  }
  tau.p.survey <- pow(sd.p.survey, -2)
  sd.p.survey ~ dunif(0, 1) # site-survey heterogeneity in p
  # Poisson-lognormal model for abundance
  for (i in 1:nsite){
    eps.lam[i] ~ dnorm(0, tau.lam[i]) # Random site effects in log(abundance)
    loglam[i] <- beta0 + inprod(beta[], lamDM[i,]) + eps.lam[i]
    loglam.lim[i] <- min(250, max(-250, loglam[i])) # 'Stabilize' log
    mu.poisson[i] <- exp(loglam.lim[i])
    N[i] ~ dpois(mu.poisson[i])
  }
  
  # Binomial measurement error model with extra-binomial dispersion
  for (i in 1:nsite){
    for (j in 1:nrep){
      y[i,j] ~ dbin(p[i,j], N[i])
      p[i,j] <- 1 / (1 + exp(-lp.lim[i,j]))
      lp.lim[i,j] <- min(250, max(-250, lp[i,j])) # 'Stabilize' logit
      lp[i,j] <- alpha0[j] + alpha[1] * elev[i] + alpha[2] * elev2[i] +
        alpha[3] * date[i,j] + alpha[4] * date2[i,j] +
        alpha[5] * dur[i,j] + alpha[6] * dur2[i,j] +
        alpha[7] * elev[i] * date[i,j] + alpha[8] * elev2[i] * date[i,j] +
        alpha[9] * elev[i] * dur[i,j] + alpha[10] * elev[i] * dur2[i,j] +
        alpha[11] * elev2[i] * dur[i,j] + alpha[12] * date[i,j] * dur[i,j] +
        alpha[13] * date[i,j] * dur2[i,j] + eps.p.survey[i,j]
      eps.p.survey[i,j] ~ dnorm(0, tau.p.survey) # Random site-survey effects
    }
  }
}