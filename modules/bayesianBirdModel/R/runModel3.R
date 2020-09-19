runModel3 <- function(birdData, # WE DECIDED TO GO FOR INLA WITH THIS --> NEED TO CONVERT THIS SCRIPT
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
      # classInd <- LCCIndex[i]
      BCR_PROV_LCCInd <- BCR_PROV_LCC[i]
      # each sample point / each row of the data table
      # Local scale
      counts[i]  ~ dpois(lambda[i])
      lambda[i] <- exp(loglambda[i])
      loglambda[i] <-
        beta[1] * State_P_500[i] + # Disturbance on neighborhood (annulus) % of forested cells in the total
        beta[2] * State_P_100[i] + # Disturbance on local (annulus) % of forested cells in the total
        beta[3] * State_P_0[i] + # Disturbance on the cell
        offsetMethods[i] +
        offsetNumberPixelsAvailable[i] +
        BCR_PROV_LCC[BCR_PROV_LCCInd] * switchCluster +
        location.Temporal + # as random effect
        YearRanEff[yearInd] * switchYear # Random Effects
      
      # REVISED PROPOSAL --> Needs to be worked on
      # 2 models
      # 
      # loglambda[i] <-
      # gamma[1] * proportionOf30YPlusForest_500[i] + # Proportion of forested areas calculated
      #   gamma[2] * proportionOf30YPlusForest_100[i] + #
      #   gamma[3] * is30YPlusForest[i] + # Disturbance on the cell -- categorical
      #   offsetMethods[i] +
      #   BCR_PROV_LCC[BCR_PROV_LCCInd] * switchCluster
      # spatioTemporal + # as random effect
      #   # YearRanEff[yearInd] * switchYear # Random Effects
      #   
      #   AND
      #   
      #   loglambda[i] <-
      #   beta[1] * proportionOfForest_500[i] + # Proportion of forested areas calculated
      #   beta[2] * proportionOfForest_100[i] + #
      #   beta[3] * isForest[i] + # Disturbance on the cell -- categorical
      #   gamma[1] * proportionOf30YPlusForest_500[i] + # Proportion of forested areas calculated
      #   gamma[2] * proportionOf30YPlusForest_100[i] + #
      #   gamma[3] * is30YPlusForest[i] + # Disturbance on the cell -- categorical
      #   offsetMethods[i] +
      #   BCR_PROV_LCC[BCR_PROV_LCCInd] * switchCluster +
      #   spatioTemporal + # as random effect
      #   # YearRanEff[yearInd] * switchYear # Random Effects
      #   
      
    } # end i
  })
  # Original model
  # counts ~ density(LCC_PROV_BCR) + disturbance100 + offsets + 1|YYYY + 1|Cluster
  # counts ~ density(LCC_PROV_BCR) + disturbance500 + offsets + 1|YYYY + 1|Cluster
  ################################### MODEL
  
  # Data (doesn't need init)
  iSMdata <-
    list(
      counts = birdData$counts,
      # LCCIndex = birdData$LCC, # Need to create
      # BCR = birdData$BCR, # Need to create
      # Prov = birdData$Prov, # Need to create
      State_P_100 = birdData$State_P_100,
      State_P_500 = birdData$State_P_500,
      offset = birdData$offset,
      location.Temporal = birdData$location.Temporal, # need to create
      BCR_PROV_LCC = birdData$ClusterSP,
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