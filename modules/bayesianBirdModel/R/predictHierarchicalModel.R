predictHierarchicalModel <- function(bird,
                                     birdList,
                                     currentYearBirdData,
                                     currentTime,
                                     pathData, 
                                     modelType = 2){
  startTime <- Sys.time()
  birdSp <- birdList[[bird]]
  message(crayon::magenta(paste0("Predictions for ", crayon::yellow(birdSp), 
                                 " for year ", currentTime, 
                                 ". (Time: ", startTime,")")))
  # Change Counts, Offset and density
  names(currentYearBirdData)[names(currentYearBirdData) == birdSp] <- "counts"
  names(currentYearBirdData)[names(currentYearBirdData) == grepMulti(x = names(currentYearBirdData), patterns = "OFFSET")] <- "offset"
  names(currentYearBirdData)[names(currentYearBirdData) == grepMulti(x = names(currentYearBirdData), patterns = "DENSITY")] <- "density"
  currentYearBirdData <- na.omit(currentYearBirdData)
  
  # Integer Cluster and Year.
  currentYearBirdData[, ClusterSP := as.integer(factor(ClusterSP))]
  currentYearBirdData[, YYYY := as.integer(factor(YYYY))] # 2000:2010, then its 1:10

  # MODEL CALL nimbleModel
  library("nimble")
  startTime <- Sys.time()

  if (modelType == 1){
    ################################### 
    #MODEL 1: 1 equation + ZI
    iSMCode <- nimbleCode({
      
      ######## PRIORS ######## ---- MODEL 1
      
      #Zero inflation - habitat suitability
      phi ~ dunif(0, 1) # proportion of suitable sites (we need to keep phi between 0 and 1)
      # Coefficient for disturbance
      mu.beta ~ dnorm(0, 100)
      tau.beta <- pow(sd.beta, -2)
      sd.beta ~ dunif(0, 100) # cluster heterogeneity in lambda
      for (b in 1:Nbeta) {
        beta[b] ~ dnorm(mu.beta, tau.beta) # Hyperparameter for beta coefficients # No idea what to put here!
      }
      # Random effect
      tau.clusters <- pow(sd.clusters, -2) # year heterogeneity in lambda
      sd.cluster ~ dunif(0, 100)
      tau.year <- pow(sd.year, -2) # cluster heterogeneity in lambda
      sd.year ~ dunif(0, 100)
      for (j in 1:NClusters) {
        clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.clusters
        # random cluster effects in log(density)
      }
      for (y in 1:NYears) {
        YearRanEff[y] ~ dnorm(0, tau.year)
        # random year effects in log(density)
      }
      
      ##### END OF PRIORS ####
      
      for (i in 1:nvisits){ # each sample point / each row of the data table
        omega[i] ~ dbern(phi) # Habitat suitability --> zero inflation (if phi = 0, unsuitable)
        # Local scale
        yearInd <- YearIndex[i]
        clusterInd <- ClusterIndex[i]
        counts[i]  ~ dpois(mu.poisson[i])
        mu.poisson[i] <- omega[i] * lambda[i] 
        lambda[i] <- exp(loglambda[i])
        loglambda[i] <- logDensity[i] + beta[1] * State_P_500[i] + beta[2] * State_P_100[i] + offset[i] + 
          clusterRanEff[clusterInd] * switchCluster +
          YearRanEff[yearInd] * switchYear # Random Effects
      } # end i
    })
    
    ################################### MODEL
    
    # Data (doesn't need init)
    iSMdata <- list(counts = currentYearBirdData$counts,  
                    logDensity = currentYearBirdData$density,
                    State_P_100 = currentYearBirdData$State_P_100,
                    State_P_500 = currentYearBirdData$State_P_500,
                    offset = currentYearBirdData$offset,
                    ClusterIndex = currentYearBirdData$ClusterSP,
                    YearIndex = currentYearBirdData$YYYY)
    # Constants
    nvisits <- NROW(currentYearBirdData)
    NClusters <- length(unique(currentYearBirdData$ClusterSP))
    NYears <- length(unique(currentYearBirdData$YYYY))
    Nbeta <- 2 # Number of different beta coefficients (coming from the same hyperparameter)
    iSMconstants <- list(nvisits = nvisits,
                         NClusters = NClusters,
                         NYears = NYears,
                         Nbeta = Nbeta, # number of coefficients, currently: beta[1] and beta[2]
                         # Switches
                         switchYear = 1, # or 0 to turn off random effects of Year
                         switchCluster = 1 # or 0 to turn off random effects of Cluster
    )
    
    iSMinits <- list(
      # Coefficients
      phi = runif(1, 0, 1), # ZI part (‘suitability’ of the sample site).
      beta = runif(Nbeta, -10, 10), # Coefficient for disturbance ~ Has distribution, not sure need to provide
      omega = rep(1, times = nvisits), # Bernoulli of phi, sample site ‘suitability’ ~ Has distribution, not sure need to provide
      sd.year = runif(1, 0, 100), # Deviation of year RE ~ Has distribution
      sd.clusters = runif(1, 0, 100), # Deviation of cluester RE ~ Has distribution
      sd.beta = runif(1, 0, 100), # Deviation of beta hyperparameter ~ Has distribution
      mu.beta = runif(1, 0, 100),
      YearRanEff = rep(0, times = NYears),
      clusterRanEff = rep(0, times = NClusters)
    )

    params <- c("beta", "mu.beta", "lambda", "omega", "phi", "tau.beta",
                "clusterRanEff", "YearRanEff", "mu.poisson")
  }
  
  if (modelType == 2){
    ################################### MODEL 2: 2 equations + ZI
    iSMCode <- nimbleCode({
      
      ######## PRIORS ########
      #Zero inflation - habitat suitability
      phi ~ dunif(0, 1) # proportion of suitable sites (we need to keep phi between 0 and 1)
      
      # Coefficient for disturbance
      mu.beta ~ dnorm(0, 100)
      sd.beta ~ dunif(0, 100)
      tau.beta <- pow(sd.beta, -2) # cluster heterogeneity in lambda
      for (b in 1:Nbeta) {
        beta[b] ~ dnorm(mu.beta, tau.beta) # Hyperparameter for beta coefficients # No idea what to put here!
      }
      sd.NN ~ dunif(0, 100)
      tau.NN <- pow(sd.NN, -2) # cluster heterogeneity in NN
      
      # Random effect
      sd.cluster ~ dunif(0, 100)
      tau.clusters <- pow(sd.clusters, -2) # year heterogeneity in lambda
      sd.year  ~ dunif(0, 100)
      tau.year <- pow(sd.year, -2) # cluster heterogeneity in lambda
      for (j in 1:NClusters) {
        clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.clusters
        # random cluster effects in log(density)
      }
      for (y in 1:NYears) {
        YearRanEff[y] ~ dnorm(0, tau.year)
        # random year effects in log(density)
      }
      
      ##### END OF PRIORS ####
      
      for (i in 1:nvisits){ # each sample point / each row of the data table
        omega[i] ~ dbern(phi) # Habitat suitability --> zero inflation (if phi = 0, unsuitable)
        # Neighborhood scale
        yearInd <- YearIndex[i]
        clusterInd <- ClusterIndex[i]
        
        logNN[i] ~ dnorm(loglambdaN[i], tau.NN)
        loglambdaN[i] <- logDensity[i] +
          beta[1] * State_P_500[i] +
          offset[i] +
          clusterRanEff[clusterInd] * switchCluster +
          YearRanEff[yearInd] * switchYear # Random Effects
        
        # Local scale
        counts[i]  ~ dpois(lambdaLOmega[i])
        lambdaLOmega[i] <- omega[i] * lambdaL[i]
        lambdaL[i] <- exp(loglambdaL[i])
        loglambdaL[i] <- logNN[i] + beta[2] * State_P_100[i] # We can add an interceptL to NN?
      } # end i
    })
    
    ################################### MODEL
    
    # Data (doesn't need init)
    iSMdata <- list(counts = currentYearBirdData$counts,  
                    logDensity = currentYearBirdData$density, # Its says density, but it really is logDensity
                    State_P_100 = currentYearBirdData$State_P_100,
                    State_P_500 = currentYearBirdData$State_P_500,
                    offset = currentYearBirdData$offset,
                    ClusterIndex = currentYearBirdData$ClusterSP,
                    YearIndex = currentYearBirdData$YYYY)
    # Constants
    nvisits <- NROW(currentYearBirdData)
    NClusters <- length(unique(currentYearBirdData$ClusterSP))
    NYears <- length(unique(currentYearBirdData$YYYY))
    Nbeta <- 2 # Number of different beta coefficients (coming from the same hyperparameter)
    iSMconstants <- list(nvisits = nvisits,
                         NClusters = NClusters,
                         NYears = NYears,
                         Nbeta = Nbeta, # number of coefficients, currently: beta[1] and beta[2]
                         # Switches
                         switchYear = 1, # or 0 to turn off random effects of Year
                         switchCluster = 1 # or 0 to turn off random effects of Cluster
    )
    
    lambdaL <- runif(1, 0, 5)# Abundance. Needs reasonable values between 0 and 5?
    iSMinits <- list(
      # Coefficients
      phi = runif(1, 0, 1), # ZI part (‘suitability’ of the sample site).
      beta = runif(Nbeta, -10, 10), # Coefficient for disturbance ~ Has distribution, not sure need to provide
      omega = rep(1, times = nvisits), # Bernoulli of phi, sample site ‘suitability’ ~ Has distribution, not sure need to provide
      logNN = rep(1, times = nvisits), # Distribution of lambda1 ~ Has distribution, not sure need to provide
      lambdaLOmega = currentYearBirdData$counts, # Distribution of counts <- Is assigned, not sure need to provide # REMOVED FOR NOW --> Simplified
      lambdaL = lambdaL, # Local model, poisson of counts <- Is assigned
      loglambdaL = log(lambdaL), # log of lambda2
      sd.year = runif(1, 0, 100), # Deviation of year RE ~ Has distribution
      sd.clusters = runif(1, 0, 100), # Deviation of cluester RE ~ Has distribution
      sd.beta = runif(1, 0, 100), # Deviation of beta hyperparameter ~ Has distribution
      mu.beta = runif(1, 0, 100),
      tau.NN = runif(1, 0, 100), # Deviation of logNN
      YearRanEff = rep(0, times = NYears),
      clusterRanEff = rep(0, times = NClusters)
    )
    params <- c("beta", "mu.beta", "loglambdaN", "sd.year", "sd.cluster", 
                "sd.beta", "sd.NN", "tau.beta", "tau.NN",
                "logNN", "lambdaL", "omega", "lambdaLOmega",
                "phi", "clusterRanEff", "YearRanEff")
  }
  
  if (modelType == 3){
    ################################### MODEL 3: 1 equation no ZI
    iSMCode <- nimbleCode({
      
      ######## PRIORS ########
      #Zero inflation - habitat suitability
      
      # Coefficient for disturbance
      mu.beta ~ dnorm(0, 100)
      sd.beta ~ dunif(0, 100)
      tau.beta <- pow(sd.beta, -2) # cluster heterogeneity in lambda
      for (b in 1:Nbeta) {
        beta[b] ~ dnorm(mu.beta, tau.beta) # Hyperparameter for beta coefficients # No idea what to put here!
      }
      
      # Random effect
      sd.cluster ~ dunif(0, 100)
      tau.clusters <- pow(sd.clusters, -2) # year heterogeneity in lambda
      sd.year ~ dunif(0, 100)
      tau.year <- pow(sd.year, -2) # cluster heterogeneity in lambda
      for (j in 1:NClusters) {
        clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.clusters
        # random cluster effects in log(density)
      }
      for (y in 1:NYears) {
        YearRanEff[y] ~ dnorm(0, tau.year)
        # random year effects in log(density)
      }
      
      ##### END OF PRIORS ####
      
      for (i in 1:nvisits){ # each sample point / each row of the data table
        # Local scale
        yearInd <- YearIndex[i]
        clusterInd <- ClusterIndex[i]
        counts[i]  ~ dpois(lambda[i])
        lambda[i] <- exp(loglambda[i])
        loglambda[i] <- logDensity[i] + beta[1] * State_P_500[i] + beta[2] * State_P_100[i] + offset[i] + 
          clusterRanEff[clusterInd] * switchCluster +
          YearRanEff[yearInd] * switchYear # Random Effects
      } # end i
    })
    
    ################################### MODEL
    
    # Data (doesn't need init)
    iSMdata <- list(counts = currentYearBirdData$counts,  
                    logDensity = currentYearBirdData$density,
                    State_P_100 = currentYearBirdData$State_P_100,
                    State_P_500 = currentYearBirdData$State_P_500,
                    offset = currentYearBirdData$offset,
                    ClusterIndex = currentYearBirdData$ClusterSP,
                    YearIndex = currentYearBirdData$YYYY)
    # Constants
    nvisits <- NROW(currentYearBirdData)
    NClusters <- length(unique(currentYearBirdData$ClusterSP))
    NYears <- length(unique(currentYearBirdData$YYYY))
    Nbeta <- 2 # Number of different beta coefficients (coming from the same hyperparameter)
    iSMconstants <- list(nvisits = nvisits,
                         NClusters = NClusters,
                         NYears = NYears,
                         Nbeta = Nbeta, # number of coefficients, currently: beta[1] and beta[2]
                         # Switches
                         switchYear = 1, # or 0 to turn off random effects of Year
                         switchCluster = 1 # or 0 to turn off random effects of Cluster
    )
    
    iSMinits <- list(
      # Coefficients
      beta = runif(Nbeta, -10, 10), # Coefficient for disturbance ~ Has distribution, not sure need to provide
      sd.year = runif(1, 0, 100), # Deviation of year RE ~ Has distribution
      sd.clusters = runif(1, 0, 100), # Deviation of cluester RE ~ Has distribution
      sd.beta = runif(1, 0, 100), # Deviation of beta hyperparameter ~ Has distribution
      mu.beta = runif(1, 0, 100),
      YearRanEff = rep(0, times = NYears),
      clusterRanEff = rep(0, times = NClusters)
    )
    
    params <- c("beta", "mu.beta", "lambda", "tau.beta",
                "clusterRanEff", "YearRanEff")
  }
  
  if (modelType == 4){
    ################################### MODEL 4: 2 equations no ZI
    iSMCode <- nimbleCode({
      
      ######## PRIORS ########
      # Coefficient for disturbance
      mu.beta ~ dnorm(0, 100)
      sd.beta ~ dunif(0, 100)
      tau.beta <- pow(sd.beta, -2) # cluster heterogeneity in lambda
      for (b in 1:Nbeta) {
        beta[b] ~ dnorm(mu.beta, tau.beta) # Hyperparameter for beta coefficients # No idea what to put here!
      }
      sd.NN ~ dunif(0, 100)
      tau.NN <- pow(sd.NN, -2) # cluster heterogeneity in NN
      
      # Random effect
      sd.cluster ~ dunif(0, 100)
      tau.clusters <- pow(sd.clusters, -2) # year heterogeneity in lambda
      sd.year ~ dunif(0, 100)
      tau.year <- pow(sd.year, -2) # cluster heterogeneity in lambda
      for (j in 1:NClusters) {
        clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.clusters
        # random cluster effects in log(density)
      }
      for (y in 1:NYears) {
        YearRanEff[y] ~ dnorm(0, tau.year)
        # random year effects in log(density)
      }
      
      ##### END OF PRIORS ####
      
      for (i in 1:nvisits){ # each sample point / each row of the data table
        # Neighborhood scale
        yearInd <- YearIndex[i]
        clusterInd <- ClusterIndex[i]
        
        logNN[i] ~ dnorm(loglambdaN[i], tau.NN)
        loglambdaN[i] <- logDensity[i] +
          beta[1] * State_P_500[i] +
          offset[i] +
          clusterRanEff[clusterInd] * switchCluster +
          YearRanEff[yearInd] * switchYear # Random Effects
        
        # Local scale
        counts[i]  ~ dpois(lambdaL[i])
        lambdaL[i] <- exp(loglambdaL[i])
        loglambdaL[i] <- logNN[i] + beta[2] * State_P_100[i] # We can add an interceptL to NN?
      } # end i
    })
    
    ################################### MODEL
    
    # Data (doesn't need init)
    iSMdata <- list(counts = currentYearBirdData$counts,  
                    logDensity = currentYearBirdData$density,
                    State_P_100 = currentYearBirdData$State_P_100,
                    State_P_500 = currentYearBirdData$State_P_500,
                    offset = currentYearBirdData$offset,
                    ClusterIndex = currentYearBirdData$ClusterSP,
                    YearIndex = currentYearBirdData$YYYY)
    # Constants
    nvisits <- NROW(currentYearBirdData)
    NClusters <- length(unique(currentYearBirdData$ClusterSP))
    NYears <- length(unique(currentYearBirdData$YYYY))
    Nbeta <- 2 # Number of different beta coefficients (coming from the same hyperparameter)
    iSMconstants <- list(nvisits = nvisits,
                         NClusters = NClusters,
                         NYears = NYears,
                         Nbeta = Nbeta, # number of coefficients, currently: beta[1] and beta[2]
                         # Switches
                         switchYear = 1, # or 0 to turn off random effects of Year
                         switchCluster = 1 # or 0 to turn off random effects of Cluster
    )
    
    lambdaL <- runif(1, 0, 5)# Abundance. Needs reasonable values between 0 and 5?
    iSMinits <- list(
      # Coefficients
      beta = runif(Nbeta, -10, 10), # Coefficient for disturbance ~ Has distribution, not sure need to provide
      logNN = rep(1, times = nvisits), # Distribution of lambda1 ~ Has distribution, not sure need to provide
      lambdaL = lambdaL, # Local model, poisson of counts <- Is assigned
      loglambdaL = log(lambdaL), # log of lambda2
      sd.year = runif(1, 0, 100), # Deviation of year RE ~ Has distribution
      sd.clusters = runif(1, 0, 100), # Deviation of cluester RE ~ Has distribution
      sd.beta = runif(1, 0, 100), # Deviation of beta hyperparameter ~ Has distribution
      mu.beta = runif(1, 0, 100),
      tau.NN = runif(1, 0, 100), # Deviation of logNN
      YearRanEff = rep(0, times = NYears),
      clusterRanEff = rep(0, times = NClusters)
    )
    params <- c("beta", "mu.beta", "loglambdaN", 
                "logNN", "lambdaL", "sd.year", "sd.cluster", 
                "sd.beta", "sd.NN", "tau.NN", "tau.beta",
                "clusterRanEff", "YearRanEff")
  }
  
  message("Starting model: ", Sys.time())
  # Build Model
  iSM <- nimbleModel(code = iSMCode,
                     constants = iSMconstants,
                     data = iSMdata,
                     inits = iSMinits,
                     name = "isM",
                     check = TRUE)
  
  message("Starting MCMC: ", Sys.time())
  mcmc.out <- nimbleMCMC(code = iSM, # It compiles internally, apparently
                         constants = iSMconstants, 
                         data = iSMdata, 
                         inits = iSMinits,
                         progressBar = TRUE,
                         monitors = params,
                         thin = 10,
                         nburnin = 1000,
                         nchains = 3, niter = 7500, # 10000
                         summary = TRUE, WAIC = FALSE)
  
  message(crayon::green(paste0("Predictions for ", crayon::yellow(birdSp), 
                               " for year ", currentTime, 
                               " finished. (Total time: ", Sys.time() - startTime,")")))
  
  return(mcmc.out)
}