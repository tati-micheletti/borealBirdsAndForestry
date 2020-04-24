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
  currentYearBirdData[["originalData"]]$ID <- FALSE
  currentYearBirdData[["originalData"]] <- na.omit(currentYearBirdData[["originalData"]])
  whichCols <- setdiff(names(currentYearBirdData[["predictionData"]]), c("ClusterSP","BIRD","offset"))
  currentYearBirdData[["predictionData"]] <- na.omit(currentYearBirdData[["predictionData"]], cols = whichCols)
  currentYearBirdData[["originalData"]] <- currentYearBirdData[["originalData"]][,.SD[sample(.N, min(50,.N))], by = YYYY]
  currentYearBirdData[["originalData"]]$ID <- NA
  predictionData1 <- currentYearBirdData[["predictionData"]][1:(NROW(currentYearBirdData[["predictionData"]])/2)]
  predictionData2 <- currentYearBirdData[["predictionData"]][1:(NROW(currentYearBirdData[["predictionData"]])/2)]
  dt <- rbind(currentYearBirdData[["originalData"]], predictionData1)
  # dt <- rbindlist(currentYearBirdData, use.names = TRUE)
  attach(dt)
  # Not sure this is correct. Will try bypassing it: Figured it out. It is just a "shortcut" on passing the
  # models. I prefered not to do that and pass the whole model inside the bugs code.
  # N.neig <- stats::model.matrix(~ State_P_500 + (1-State_P_500) * correctedAge + density)[,-1]
  # N.loc <- stats::model.matrix(~ State_P_100 + (1-State_P_100) * correctedAge)[,-1]
  
  # MODEL CALL nimbleModel
  library("nimble")
startTime <- Sys.time()
  iSMCode <- nimbleCode({
    # Preparing cluster factors
    for (j in 1:NClusters) {
      clusterRanEff[j] ~ dnorm(0, tau.clusters) # Note plural on tau.clusters, different than tau.cluster
    }
    for (y in 1:NYears) {
      YearRanEff[y] ~ dnorm(0, tau.year)
    }
    for (b in 1:Nbeta) {
      beta[b] ~ dnorm(mu.beta, tau.beta)
    }
    #N is abundance, i is the sample (data row), k is the beta parameter
    # Neighborhood scale
    for (i in 1:nvisits){ # each sample point / each row of the data table
      # tau.cluster <- pow(sd.cluster, -2)
      # sd.cluster ~ dunif(0, 2) # year heterogeneity in lambda
      # tau.year <- pow(sd.year, -2)
      # sd.year ~ dunif(0, 2) # cluster heterogeneity in lambda
      # Cluster[i] ~ dnorm(0, tau.cluster) # random cluster effects in log(density) # Instead of the 0, I 
      # Year[i] ~ dnorm(0, tau.year) # random year effects in log(density)
      NN[i]  ~ dpois(lambda1[i]) # we can also remove the NN, put the N equation inside the local one
      lambda1[i] <- density[i] + 
                    beta[1] * State_P_500[i] + 
                    # betaN[3]*(1-State_P_500[i])*sqrt(correctedAge[i]) +
                    offset[i] +
                    clusterRanEff[ClusterIndex[i]] * switchCluster + 
                    YearRanEff[YearIndex[i]] * switchYear # Random Effects
                    # Cluster[i] * switchCluster + Year[i] * switchYear # Random Effects
      
      # Local scale
      omega[i] ~ dbern(phi) # ZI part (‘suitability’ of the sample site). Phi = 0, habitat is not suitable; phi = 1, is suitable
      mu.poisson[i] <- omega[i] * lambda2[i]
      counts[i]  ~ dpois(mu.poisson[i])
      lambda2[i] <- betaL[1] * NN[i] + beta[2] * State_P_100[i] #+ betaL[2]*(1-State_P_100[i])*sqrt(correctedAge[i])
      # Maybe add a coefficient for NN[i]
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
      
      # Factorial of this
      # 1) as is
      # 2) We might take NN out and just put one equation in the other and remove betaL[1]
      # 3) Multivariate normal for beta: b ~ mvnorm(mu.beta, omega.beta) where omega.beta is a matrix? Check
      
    } # end i
    })

  # Inits
  switchYear <- 0 # or 0 to turn off random effects of Year
  switchCluster <- 0 # or 0 to turn off random effects of Cluster
  betaN <- betaL <- numeric()
  for (k in 1:3) {  # regression params in lambda N
    betaN[k] <- rnorm(1, 0, 1)}
  for (k in 1:2) {  # regression params in lambda L
    betaL[k] <- rnorm(1, 0, 1)}
  
  # All variables that have NA's in data need to be "filled in" to become inits
  offset2 <- fillUpNa(offset)
  correctedAge2 <- round(fillUpNa(correctedAge), 0)
  State_P_1002 <- fillUpNa(State_P_100)
  State_P_5002 <- fillUpNa(State_P_500)
  BIRD2 <- BIRD
  BIRD2[is.na(BIRD2)] <- 1
  
  nvisits <- NROW(density)
  # Set up the inits
  iSMinits <- list(phi = rbinom(1, 1, 0.5), 
                   beta0N = rnorm(1, 0, 0.1),
                   beta0L = rnorm(1, 0, 0.1),
                   betaN = betaN, 
                   betaL = betaL,
                   State_P_100 = State_P_1002,
                   State_P_500 = State_P_5002,
                   correctedAge = correctedAge2,
                   offset = offset2,
                   omega = rep(1, times = nvisits),
                   mu.poisson = rep(2, times = nvisits),
                   NN = rep(1, times = nvisits),
                   lambda2 = rep(1, times = nvisits),
                   # sd.year = runif(1, 0, 1),
                   # sd.cluster = runif(1, 0, 1),
                   # Cluster = ClusterSP,
                   # switchCluster = switchCluster,
                   # Year = YYYY,
                   # switchYear = switchYear,
                   counts = BIRD2
                   )
  
  # Data and constants
  iSMdata <- list(counts = BIRD,  density = density,
                  State_P_100 = State_P_100,
                  State_P_500 = State_P_500,
                  correctedAge = correctedAge,
                  offset = offset)
  
  iSMconstants <- list(nvisits = nvisits)
  
  message("Starting model: ", Sys.time())
  
  # Build Model
  iSM <- nimbleModel(code = iSMCode,
                     constants = iSMconstants,
                     data = iSMdata,
                     inits = iSMinits,
                     name = "isM",
                     check = TRUE)

  # Parameters monitored
  # params <- c("phi", "omega", "beta0N", "beta0L", "betaN", "betaL", "counts",
  #             "lambda1", "lambda2", "NN"
  #             # "fit.actual", "fit.sim", "bpv", "c.hat"
  # )
    params <- c("counts", "omega", "beta0N", "beta0L", "betaN", "betaL")
  message("Starting compilation: ", Sys.time())

  iSMcompiled <- compileNimble(iSM)

  message("Starting MCMC: ", Sys.time())
  mcmc.out <- nimbleMCMC(code = iSMcompiled, 
                         constants = iSMconstants, 
                         data = iSMdata, 
                         inits = iSMinits,
                         monitors = params,
                         nchains = 2, niter = 10000,
                         summary = TRUE, WAIC = FALSE)
  browser()
  message(crayon::green(paste0("Predictions for ", crayon::yellow(birdSp), 
                                 " for year ", currentTime, 
                                 " finished. (Total time: ", Sys.time() - startTime,")")))
  
  return(baysModels)
}