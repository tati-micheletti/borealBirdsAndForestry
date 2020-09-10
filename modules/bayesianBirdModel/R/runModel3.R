runModel3 <- function(birdData, # NOT FINISHED! Added intercept to the formula, need init and prior # <~~ Seems to be outdated?
                      bird, startTime, currentTime){
  
  message("Running model 3 multivariate model, BCR+LCC covars for ", bird)
  iSMCode <- nimbleCode({
    
    ######## PRIORS ########

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Coefficient for disturbance
    
    mean.mu.beta ~ dunif(-40, 40)
    for (scale in 1:Nbeta) {
      mu.beta[scale] ~ dnorm(mean.mu.beta, 0.1)
    }
    R[1:2, 1:2] ~ dwish(omega.beta[, ], 2)
    beta[1:2] ~ dmnorm(mu.beta[], R[,])
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Random effect
    
    tau.clusters ~ T(dgamma(1, 1), 0.01, 100)
    sd.cluster <- 1 / pow(tau.clusters, -2) # year heterogeneity in lambda
    for (j in 1:NClusters) {
      clusterRanEff[j] ~ dnorm(0, tau.clusters)
      # random cluster effects in log(density)
    }
    tau.year ~ T(dgamma(1, 1), 0.01, 100)
    sd.year <- 1 / pow(tau.year, -2) # cluster heterogeneity in lambda
    for (y in 1:NYears) {
      YearRanEff[y] ~ dnorm(0, tau.year)
      # random year effects in log(density)
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Density -- BCR PROVINCE LCC
    
    tau.BCR ~ T(dgamma(1, 1), 0.01, 100)
    sd.BCR <- 1 / pow(tau.BCR, -2)

    tau.Prov ~ T(dgamma(1, 1), 0.01, 100)
    sd.Prov <- 1 / pow(tau.Prov, -2)
    
    tau.LCC ~ T(dgamma(1, 1), 0.01, 100)
    sd.LCC <- 1 / pow(tau.LCC, -2)
    
    for (i in 1:nvisits) {
      indBCR <- BCR[i] # --> BCR is DATA, the whole vector of BCR of all visits
      BRCvec[indBCR] ~ dnorm(0, tau.BCR) # --> BCRvec is the vector of all BCRs available
      BRCvecInd <- BRCvec[indBCR]
      
      indProv <- Prov[i] # --> Prov is DATA
      ProvVec[indProv] ~ dnorm(BRCvecInd, tau.Prov)  # --> ProvVec is the vector of all Prov's available
      ProvVecInd <- ProvVec[indProv]
      
      indLCC <- LCCIndex[i] # --> LCC is DATA
      LCCvec[indLCC] ~ dnorm(ProvVecInd, tau.LCC)  # --> LCCvec is the vector of all LCC available 
    }

    ##### END OF PRIORS ####
    
    for (i in 1:nvisits) {
      # Indices
      yearInd <- YearIndex[i]
      classInd <- LCCIndex[i]
      clusterInd <- ClusterIndex[i]
      # each sample point / each row of the data table
      # Local scale
      counts[i]  ~ dpois(lambda[i])
      lambda[i] <- exp(loglambda[i])
      loglambda[i] <-
        LCCvec[classInd] +
        beta[1] * State_P_500[i] + 
        beta[2] * State_P_100[i] + 
        offset[i] +
        clusterRanEff[clusterInd] * switchCluster +
        YearRanEff[yearInd] * switchYear # Random Effects
    } # end i
  })
  
  ################################### MODEL
  
  # Data (doesn't need init)
  iSMdata <-
    list(
      counts = birdData$counts,
      LCCIndex = birdData$LCC, # Need to create
      BCR = birdData$BCR, # Need to create
      Prov = birdData$Prov, # Need to create
      State_P_100 = birdData$State_P_100,
      State_P_500 = birdData$State_P_500,
      offset = birdData$offset,
      ClusterIndex = birdData$ClusterSP,
      YearIndex = birdData$YYYY
    )
  # Constants
  nvisits <- NROW(birdData)
  NClusters <- length(unique(birdData$ClusterSP))
  NYears <- length(unique(birdData$YYYY))
  Nbeta <- 2 # Number of different beta coefficients (coming from the same hyperparameter)
  iSMconstants <- list(
    nvisits = nvisits,
    NClusters = NClusters,
    NYears = NYears,
    Nbeta = Nbeta,
    # number of coefficients, currently: beta[1] and beta[2]
    omega.beta = matrix(c(10, 0, 0, 10), nrow = 2),
    # Switches
    switchYear = 1, # or 0 to turn off random effects of Year
    switchCluster = 1 # or 0 to turn off random effects of Cluster
  )
  
  iSMinits <- list(
    # Coefficients
    beta = runif(Nbeta, min = -40, 40),
    tau.year = runif(1, 0.01, 10),
    tau.clusters = runif(1, 0.01, 10),
    mu.beta = runif(2, -40, 40),
    YearRanEff = runif(NYears, min = -1, 1),
    clusterRanEff = runif(NClusters, min = -1, 1),
    R = matrix(c(1, 0, 0, 1), nrow = 2),
    mean.mu.beta = runif(1, -40, 40)
  )
  
  params <-
    c("BRCvec",
      "ProvVec",
      "LCCvec",
      "mu.beta",
      "beta",
      "R",
      "lambda",
      "mean.mu.beta",
      "clusterRanEff",
      "YearRanEff",
      "sd.year",
      "sd.cluster",
      "sd.bcrProv",
      "sd.LCC",
      "mu.LCC")
  
  message("Starting model: ", Sys.time())
  # Build Model
  iSM <- nimbleModel(
    code = iSMCode,
    constants = iSMconstants,
    data = iSMdata,
    inits = iSMinits,
    name = "isM",
    check = TRUE
  )
  
  message("Starting MCMC: ", Sys.time())
  mcmc.out <-
    nimbleMCMC(
      code = iSM,
      # It compiles internally, apparently
      constants = iSMconstants,
      data = iSMdata,
      inits = iSMinits,
      progressBar = TRUE,
      monitors = params,
      thin = 7,
      nburnin = 1000,
      nchains = 3,
      niter = 10000,
      summary = TRUE,
      WAIC = FALSE
    )
  
  message(crayon::green(
    paste0(
      "Predictions for ",
      crayon::yellow(bird),
      " for year ",
      currentTime,
      " finished. (Total time: ",
      Sys.time() - startTime,
      ")"
    )
  ))
  
  return(mcmc.out)
  
}