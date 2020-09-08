predictHierarchicalModel <- function(bird,
                                     birdList,
                                     currentYearBirdData,
                                     currentTime,
                                     pathData, 
                                     modelType = 1,
                                     reRunModels = FALSE,
                                     useFuture = FALSE){
  startTime <- Sys.time()
  birdSp <- birdList[[bird]]
  message(crayon::magenta(paste0("Fitting model for ", crayon::yellow(birdSp), 
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
  lapplyFun <- ifelse(useFuture, future_lapply, lapply)
  mcmc.out <- do.call(lapplyFun, args = list(X = modelType, FUN = function(modType){
      modelRDSpath <- file.path(Paths$outputPath, paste0("modelBay", modType, "_", birdSp, ".rds"))
      if (all(file.exists(modelRDSpath), !isTRUE(reRunModels))){
        mcmc <- readRDS(modelRDSpath)
      } else {
        mcmc <- do.call(paste0("runModel", modType), args = list(birdData = currentYearBirdData, 
                                                         bird = birdSp,
                                                         startTime = startTime,
                                                         currentTime = currentTime))
      }
    return(mcmc)
  })
  )
  names(mcmc.out) <- paste0("model", modelType)

  return(mcmc.out)
}
