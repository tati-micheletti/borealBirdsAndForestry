predictHierarchicalModel <- function(bird,
                                     birdList,
                                     currentYearBirdData,
                                     currentTime,
                                     pathData){
  startTime <- Sys.time()
  birdSp <- birdList[[bird]]
  message(crayon::magenta(paste0("Predictions for ", crayon::yellow(birdSp), 
                                 " for year ", currentTime, 
                                 ". (Time: ", startTime,")")))
  whichToChange <- grepl(x = names(currentYearBirdData[[1]]), pattern = birdSp)
  currentYearBirdData <- lapply(X = currentYearBirdData, FUN = function(each){
    names(each)[whichToChange] <- c("density","BIRD","offset")
    return(each)
  })
  # currentYearBirdDataOrig <- currentYearBirdData # TEMP just until I am sure not screwing up the object
  # currentYearBirdData <- currentYearBirdDataOrig
  # Exclude all rows without DATA from both original and prediction Data; subset original data to 500 rows 
  # (improve the model convergence / time).
  
  currentYearBirdData <- currentYearBirdData[["originalData"]]
  
  currentYearBirdData[, ID := NULL]
  currentYearBirdData <- na.omit(currentYearBirdData)
  # whichCols <- setdiff(names(currentYearBirdData[["predictionData"]]), c("ClusterSP","BIRD","offset"))
  # currentYearBirdData[["predictionData"]] <- na.omit(currentYearBirdData[["predictionData"]], cols = whichCols)
  # currentYearBirdData <- currentYearBirdData[,.SD[sample(.N, min(50,.N))], by = YYYY]
  # currentYearBirdData$ID <- NA
  # predictionData1 <- currentYearBirdData[["predictionData"]][1:(NROW(currentYearBirdData[["predictionData"]])/2)]
  # predictionData2 <- currentYearBirdData[["predictionData"]][1:(NROW(currentYearBirdData[["predictionData"]])/2)]
  # dt <- rbind(currentYearBirdData, predictionData1)
  
  # Integer Cluster and Year. Year needs to be centered, but need to keep a copy
  originalYYYY <- currentYearBirdData$YYYY
  newYear <- data.table(YYYY = 2000:2010, nYYYY = -5:5)
  setkey(newYear, YYYY)
  setkey(currentYearBirdData, YYYY)
  currentYearBirdData <- merge(currentYearBirdData, newYear, by = "YYYY")
  currentYearBirdData[, ClusterSP := as.integer(factor(ClusterSP))]
  # dt <- rbindlist(currentYearBirdData, use.names = TRUE)
  attach(currentYearBirdData)
  # "shortcut" on passing the
  # models. I prefered not to do that and pass the whole model inside the bugs code. 
  # WIll change when have the chance
  
  # MODEL CALL nimbleModel
  library("nimble")
  startTime <- Sys.time()
  
  ################################### MODEL
  
  iSMCode <- nimbleCode({
    # Preparing cluster factors
    tau.clusters <- pow(sd.cluster, -2)
    sd.cluster ~ dunif(0, 2) # year heterogeneity in lambda
    tau.year <- pow(sd.year, -2)
    sd.year ~ dunif(0, 2) # cluster heterogeneity in lambda
    tau.beta <- pow(sd.beta, -2)
    sd.beta ~ dunif(0, 2) # cluster heterogeneity in lambda
    
    for (j in 1:NClusters) {
      clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.cluster
      # random cluster effects in log(density)
    }
    for (y in 1:NYears) {
      YearRanEff[y] ~ dnorm(0, tau.year)
      # random year effects in log(density)
    }
    mu.beta ~ dnorm(0, 0.01)
    for (b in 1:Nbeta) {
      beta[b] ~ dnorm(mu.beta, tau.beta) # Hyperparameter for beta coefficients # No idea what to put here!
    }
    #N is abundance, i is the sample (data row), k is the beta parameter
    # Neighborhood scale
    for (i in 1:nvisits){ # each sample point / each row of the data table
      NN[i] ~ dpois(lambda1[i])
      lambda1[i] <- density[i] + 
        beta[1] * State_P_500[i] + 
        offset[i] +
        clusterRanEff[ClusterIndex[i]] * switchCluster + 
        YearRanEff[YearIndex[i]] * switchYear # Random Effects
      # Local scale
      omega[i] ~ dbern(phi) # ZI part (‘suitability’ of the sample site). 
      # Phi = 0, habitat is not suitable; phi = 1, is suitable
      counts[i]  ~ dpois(lambda2[i])
      lambda2[i] <- omega[i] * 
        interceptL * NN[i] + 
        beta[2] * State_P_100[i]
      # counts[i]  ~ dpois(mu.poisson[i])
      # mu.poisson[i] <- omega[i] * lambda2[i]
      # lambda2[i] <- interceptL * NN[i] + beta[2] * State_P_100[i]
    } # end i
  })
  
  ################################### MODEL
  
  # Data
  iSMdata <- list(counts = BIRD,  
                  density = density,
                  State_P_100 = State_P_100,
                  State_P_500 = State_P_500,
                  YearIndex = nYYYY,
                  ClusterIndex = ClusterSP,
                  offset = offset)
  # Constants
  nvisits <- NROW(density)
  NClusters <- length(unique(ClusterSP))
  NYears <- length(unique(nYYYY))
  Nbeta <- 2 # Number of different beta coefficients (coming from the same hyperparameter)
  iSMconstants <- list(nvisits = NROW(density),
                       NClusters = NClusters,
                       NYears = NYears,
                       Nbeta = Nbeta # number of coefficients, currently: beta[1] and beta[2]
  )
  browser()
  # Set up the inits: All variables that have NA's in data 
  # need to be "filled in" to become inits # I MIGHT NOT NEED IF NOT PREDICTING!
  offsetinit <- offset
  State_P_100init <- State_P_100
  State_P_500init <- State_P_500
  countinit <- BIRD
  
  iSMinits <- list(
    # Switches
    switchYear = 1, # or 0 to turn off random effects of Year
    switchCluster = 1, # or 0 to turn off random effects of Cluster
    # Coefficients
    phi = rbinom(1, 1, 0.5), # ZI part (‘suitability’ of the sample site).
    interceptL = rnorm(1, 0, 0.1), # Intercept local
    beta = rep(rnorm(1, 0, 0.1), times = Nbeta), # Coefficient for disturbance ~ Has distribution, not sure need to provide
    omega = rep(1, times = nvisits), # Bernoulli of phi, sample site ‘suitability’ ~ Has distribution, not sure need to provide
    # mu.poisson = rep(2, times = nvisits), # Distribution of counts <- Is assigned, not sure need to provide # REMOVED FOR NOW --> Simplified
    NN = rep(1, times = nvisits), # Distribution of lambda1 ~ Has distribution, not sure need to provide
    lambda2 = rep(1, times = nvisits), # Local model, poisson of counts <- Is assigned
    lambda1 = rep(1, times = nvisits), # Neighborhood model, poisson of NN: effect in Local model <- Is assigned not sure need to provide
    sd.year = runif(1, 0, 1), # Deviation of year RE ~ Has distribution
    sd.cluster = runif(1, 0, 1), # Deviation of cluester RE ~ Has distribution
    sd.beta = runif(1, 0, 1), # Deviation of beta hyperparameter ~ Has distribution
    mu.beta = runif(1, 0, 1),
    # Data
    State_P_100 = State_P_100, # Disturbance proportion within 100m (local)
    State_P_500 = State_P_500,# Disturbance proportion within 500m (neighborhood)
    offset = offset, # Offsets :: convert counts into abundance
    ClusterIndex = ClusterSP, # Clustered point counts, as RE to eliminate spatial correlation 
    YearIndex = nYYYY, # Indexed years to account for "bad" and "good" years for reproduction
    counts = BIRD, # Bird counts
    YearRanEff = rep(0, times = NYears), # 
    clusterRanEff = rep(0, times = NClusters)
  )
  
  message("Starting model: ", Sys.time())
  
  # Build Model
  iSM <- nimbleModel(code = iSMCode,
                     constants = iSMconstants,
                     data = iSMdata,
                     inits = iSMinits,
                     name = "isM",
                     check = TRUE)
  
  params <- c("phi", "omega", "beta", "interceptL", "mu.beta", "YearRanEff", "clusterRanEff")
  message("Starting compilation: ", Sys.time())
  
  iSMcompiled <- compileNimble(iSM)
  
  message("Starting MCMC: ", Sys.time())
  mcmc.out <- nimbleMCMC(code = iSMcompiled, 
                         constants = iSMconstants, 
                         data = iSMdata, 
                         inits = iSMinits,
                         monitors = params,
                         nchains = 2, niter = 1000, # 10000
                         summary = TRUE, WAIC = FALSE)
  browser()
  message(crayon::green(paste0("Predictions for ", crayon::yellow(birdSp), 
                               " for year ", currentTime, 
                               " finished. (Total time: ", Sys.time() - startTime,")")))
  
  return(baysModels)
}