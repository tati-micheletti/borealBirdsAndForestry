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
      
      ######## PRIORS ########
      #Zero inflation - habitat suitability
      phi ~ dunif(0, 1) # proportion of suitable sites (we need to keep phi between 0 and 1)
      
      # Coefficient for disturbance
      mu.beta ~ dnorm(0, 0.01)
      tau.beta <- pow(sd.beta, -2)
      sd.beta ~ dunif(0, 2) # cluster heterogeneity in lambda
      for (b in 1:Nbeta) {
        beta[b] ~ dnorm(mu.beta, tau.beta) # Hyperparameter for beta coefficients # No idea what to put here!
      }
      
      # Random effect
      tau.clusters <- pow(sd.cluster, -2)
      sd.cluster ~ dunif(0, 2) # year heterogeneity in lambda
      tau.year <- pow(sd.year, -2)
      sd.year ~ dunif(0, 2) # cluster heterogeneity in lambda
      for (j in 1:NClusters) {
        clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.cluster
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
        loglambda[i] <- density[i] + beta[1] * State_P_500[i] + beta[2] * State_P_100[i] + offset[i] + 
          clusterRanEff[clusterInd] * switchCluster +
          YearRanEff[yearInd] * switchYear # Random Effects
      } # end i
    })
    
    ################################### MODEL
    
    # Data (doesn't need init)
    iSMdata <- list(counts = currentYearBirdData$counts,  
                    density = currentYearBirdData$density,
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
      # interceptL = rnorm(1, 0, 0.1), # Intercept local
      beta = rep(rnorm(1, 0, 0.1), times = Nbeta), # Coefficient for disturbance ~ Has distribution, not sure need to provide
      omega = rep(1, times = nvisits), # Bernoulli of phi, sample site ‘suitability’ ~ Has distribution, not sure need to provide
      # NN = rep(1, times = nvisits), # Distribution of lambda1 ~ Has distribution, not sure need to provide
      # mu.poisson = currentYearBirdData$counts, # Distribution of counts <- Is assigned, not sure need to provide # REMOVED FOR NOW --> Simplified
      # lambda2 = lambda2, # Local model, poisson of counts <- Is assigned 
      # loglambda2 = log(lambda2), # log of lambda2
      # lambda1 = rep(1, times = nvisits), # Neighborhood model, poisson of NN: effect in Local model <- Is assigned not sure need to provide
      sd.year = runif(1, 0, 1), # Deviation of year RE ~ Has distribution
      sd.cluster = runif(1, 0, 1), # Deviation of cluester RE ~ Has distribution
      sd.beta = runif(1, 0, 1), # Deviation of beta hyperparameter ~ Has distribution
      mu.beta = runif(1, 0, 1),
      YearRanEff = rep(0, times = NYears),
      clusterRanEff = rep(0, times = NClusters)
    )

    params <- c("beta", "mu.beta", "lambda", "omega", "phi", 
                "clusterRanEff", "YearRanEff", "mu.poisson")
  }
  
  if (modelType == 2){
    ################################### MODEL 2: 2 equations + ZI
    iSMCode <- nimbleCode({
      
      ######## PRIORS ########
      #Zero inflation - habitat suitability
      phi ~ dunif(0, 1) # proportion of suitable sites (we need to keep phi between 0 and 1)
      
      # Coefficient for disturbance
      mu.beta ~ dnorm(0, 0.01)
      tau.beta <- pow(sd.beta, -2)
      sd.beta ~ dunif(0, 2) # cluster heterogeneity in lambda
      for (b in 1:Nbeta) {
        beta[b] ~ dnorm(mu.beta, tau.beta) # Hyperparameter for beta coefficients # No idea what to put here!
      }
      
      # Random effect
      tau.clusters <- pow(sd.cluster, -2)
      sd.cluster ~ dunif(0, 2) # year heterogeneity in lambda
      tau.year <- pow(sd.year, -2)
      sd.year ~ dunif(0, 2) # cluster heterogeneity in lambda
      for (j in 1:NClusters) {
        clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.cluster
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
        
        NN[i] ~ dpois(lambdaN[i])
        lambdaN[i] <- exp(loglambdaN[i])
        
        loglambdaN[i] <- density[i] +
          beta[1] * State_P_500[i] +
          offset[i] +
          clusterRanEff[clusterInd] * switchCluster +
          YearRanEff[yearInd] * switchYear # Random Effects
        
        # Local scale
        counts[i]  ~ dpois(lambdaLOmega[i])
        lambdaLOmega[i] <- omega[i] * lambdaL[i] 
        lambdaL[i] <- exp(loglambdaL[i])
        loglambdaL[i] <- NN[i] + beta[2] * State_P_100[i] # We can add an interceptL to NN?
      } # end i
    })
    
    ################################### MODEL
    
    # Data (doesn't need init)
    iSMdata <- list(counts = currentYearBirdData$counts,  
                    density = currentYearBirdData$density,
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
      # interceptL = rnorm(1, 0, 0.1), # Intercept local
      beta = rep(rnorm(1, 0, 0.1), times = Nbeta), # Coefficient for disturbance ~ Has distribution, not sure need to provide
      omega = rep(1, times = nvisits), # Bernoulli of phi, sample site ‘suitability’ ~ Has distribution, not sure need to provide
      NN = rep(1, times = nvisits), # Distribution of lambda1 ~ Has distribution, not sure need to provide
      lambdaLOmega = currentYearBirdData$counts, # Distribution of counts <- Is assigned, not sure need to provide # REMOVED FOR NOW --> Simplified
      lambdaL = lambdaL, # Local model, poisson of counts <- Is assigned
      loglambdaL = log(lambdaL), # log of lambda2
      # lambda1 = rep(1, times = nvisits), # Neighborhood model, poisson of NN: effect in Local model <- Is assigned not sure need to provide
      sd.year = runif(1, 0, 1), # Deviation of year RE ~ Has distribution
      sd.cluster = runif(1, 0, 1), # Deviation of cluester RE ~ Has distribution
      sd.beta = runif(1, 0, 1), # Deviation of beta hyperparameter ~ Has distribution
      mu.beta = runif(1, 0, 1),
      YearRanEff = rep(0, times = NYears),
      clusterRanEff = rep(0, times = NClusters)
    )
    params <- c("beta", "mu.beta", "lambdaN", 
                "NN", "lambdaL", "omega", "lambdaLOmega",
                "phi", "clusterRanEff", "YearRanEff")
  }
  
  if (modelType == 3){
    ################################### MODEL 3: 1 equation no ZI
    iSMCode <- nimbleCode({
      
      ######## PRIORS ########
      #Zero inflation - habitat suitability
      
      # Coefficient for disturbance
      mu.beta ~ dnorm(0, 0.01)
      tau.beta <- pow(sd.beta, -2)
      sd.beta ~ dunif(0, 2) # cluster heterogeneity in lambda
      for (b in 1:Nbeta) {
        beta[b] ~ dnorm(mu.beta, tau.beta) # Hyperparameter for beta coefficients # No idea what to put here!
      }
      
      # Random effect
      tau.clusters <- pow(sd.cluster, -2)
      sd.cluster ~ dunif(0, 2) # year heterogeneity in lambda
      tau.year <- pow(sd.year, -2)
      sd.year ~ dunif(0, 2) # cluster heterogeneity in lambda
      for (j in 1:NClusters) {
        clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.cluster
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
        loglambda[i] <- density[i] + beta[1] * State_P_500[i] + beta[2] * State_P_100[i] + offset[i] + 
          clusterRanEff[clusterInd] * switchCluster +
          YearRanEff[yearInd] * switchYear # Random Effects
      } # end i
    })
    
    ################################### MODEL
    
    # Data (doesn't need init)
    iSMdata <- list(counts = currentYearBirdData$counts,  
                    density = currentYearBirdData$density,
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
      # interceptL = rnorm(1, 0, 0.1), # Intercept local
      beta = rep(rnorm(1, 0, 0.1), times = Nbeta), # Coefficient for disturbance ~ Has distribution, not sure need to provide
      # NN = rep(1, times = nvisits), # Distribution of lambda1 ~ Has distribution, not sure need to provide
      # mu.poisson = currentYearBirdData$counts, # Distribution of counts <- Is assigned, not sure need to provide # REMOVED FOR NOW --> Simplified
      # lambda2 = lambda2, # Local model, poisson of counts <- Is assigned 
      # loglambda2 = log(lambda2), # log of lambda2
      # lambda1 = rep(1, times = nvisits), # Neighborhood model, poisson of NN: effect in Local model <- Is assigned not sure need to provide
      sd.year = runif(1, 0, 1), # Deviation of year RE ~ Has distribution
      sd.cluster = runif(1, 0, 1), # Deviation of cluester RE ~ Has distribution
      sd.beta = runif(1, 0, 1), # Deviation of beta hyperparameter ~ Has distribution
      mu.beta = runif(1, 0, 1),
      YearRanEff = rep(0, times = NYears),
      clusterRanEff = rep(0, times = NClusters)
    )
    
    params <- c("beta", "mu.beta", "lambda",
                "clusterRanEff", "YearRanEff")
  }
  
  if (modelType == 4){
    ################################### MODEL 4: 2 equations no ZI
    iSMCode <- nimbleCode({
      
      ######## PRIORS ########
      # Coefficient for disturbance
      mu.beta ~ dnorm(0, 0.01)
      tau.beta <- pow(sd.beta, -2)
      sd.beta ~ dunif(0, 2) # cluster heterogeneity in lambda
      for (b in 1:Nbeta) {
        beta[b] ~ dnorm(mu.beta, tau.beta) # Hyperparameter for beta coefficients # No idea what to put here!
      }
      
      # Random effect
      tau.clusters <- pow(sd.cluster, -2)
      sd.cluster ~ dunif(0, 2) # year heterogeneity in lambda
      tau.year <- pow(sd.year, -2)
      sd.year ~ dunif(0, 2) # cluster heterogeneity in lambda
      for (j in 1:NClusters) {
        clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.cluster
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
        
        NN[i] ~ dpois(lambdaN[i])
        lambdaN[i] <- exp(loglambdaN[i])
        
        loglambdaN[i] <- density[i] +
          beta[1] * State_P_500[i] +
          offset[i] +
          clusterRanEff[clusterInd] * switchCluster +
          YearRanEff[yearInd] * switchYear # Random Effects
        
        # Local scale
        counts[i]  ~ dpois(lambdaL[i])
        lambdaL[i] <- exp(loglambdaL[i])
        loglambdaL[i] <- NN[i] + beta[2] * State_P_100[i] # We can add an interceptL to NN?
      } # end i
    })
    
    ################################### MODEL
    
    # Data (doesn't need init)
    iSMdata <- list(counts = currentYearBirdData$counts,  
                    density = currentYearBirdData$density,
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
      # interceptL = rnorm(1, 0, 0.1), # Intercept local
      beta = rep(rnorm(1, 0, 0.1), times = Nbeta), # Coefficient for disturbance ~ Has distribution, not sure need to provide
      NN = rep(1, times = nvisits), # Distribution of lambda1 ~ Has distribution, not sure need to provide
      lambdaL = lambdaL, # Local model, poisson of counts <- Is assigned
      loglambdaL = log(lambdaL), # log of lambda2
      # lambda1 = rep(1, times = nvisits), # Neighborhood model, poisson of NN: effect in Local model <- Is assigned not sure need to provide
      sd.year = runif(1, 0, 1), # Deviation of year RE ~ Has distribution
      sd.cluster = runif(1, 0, 1), # Deviation of cluester RE ~ Has distribution
      sd.beta = runif(1, 0, 1), # Deviation of beta hyperparameter ~ Has distribution
      mu.beta = runif(1, 0, 1),
      YearRanEff = rep(0, times = NYears),
      clusterRanEff = rep(0, times = NClusters)
    )
    params <- c("beta", "mu.beta", "lambdaN", 
                "NN", "lambdaL",
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
                         thin = 3,
                         nburnin = 1000,
                         nchains = 3, niter = 10000, # 10000
                         summary = TRUE, WAIC = FALSE)
  
  message(crayon::green(paste0("Predictions for ", crayon::yellow(birdSp), 
                               " for year ", currentTime, 
                               " finished. (Total time: ", Sys.time() - startTime,")")))
  
  return(mcmc.out)
}