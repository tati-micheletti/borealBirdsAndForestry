LCReclassify <- function(inputTiles = tilelist, 
                         startTime = startTime,
                         endTime = endTime,
                         forestClass = forestClass,
                         focalDistance = focalDistance,
                         disturbanceClass = disturbanceClass,
                         pathData = pathData, 
                         intermPath = intermPath,
                         spName = spName,
                         passedModel = passedModel,
                         tileNumber = tileNumber,
                         recoverTime = recoverTime,
                         maxTile = maxTile){

  #Assumed that abundance reflects landcover of non-forest as well. 
  #reclassify landcover into binary with 1 = forest
  message(crayon::red(paste0("Processing tile ",  tileNumber, " of ", 
                             maxTile, " tiles for ", spName, 
                             " (Time: ", Sys.time(), ")")))
  
  binaryLC <- binaryReclassify(inFile = inputTiles$land, inValues = forestClass)
  storage.mode(binaryLC[]) <- "integer" # Reducing size of raster by converting it to a real binary
  
  # Create focal matrix: binaryLC has 1 for forest and 0 for rest
  focalMatrices <- lapply(focalDistance, FUN = raster::focalWeight, x = binaryLC) # binaryLC is used only as template here
  
  # Calculee focal statistics with each matrix
  LCFocals <- lapply(focalMatrices, FUN = focal, x = binaryLC, na.rm = TRUE)
  
  # Calculate binary version of disturbance type
  binaryDisturb <- binaryReclassify(inFile = inputTiles$disturbanceType, inValues = disturbanceClass)
  storage.mode(binaryDisturb[]) <- "integer" # Reducing size of raster by converting it to a real binary
  
  # Generate new raster for each year, where only values disturbed in a given year or X previous years (recoverTime) are 1
  # X previous years is a parameter passed from global (we can run in experiment): recoverTime
  if (startTime > 1000 & endTime > 1000){
    startTime <- startTime - 1900
    endTime <- endTime - 1900
  }
  
  times <- c(startTime:endTime)
  
  distStack <- lapply(X = times, FUN = function(x){
    maskValue <- c((x-recoverTime):x) # Calculate cummulative effects. If not wanted, change to maskValue <- x
    jumbMasked <- jumboMask(inputRas = binaryDisturb, 
                      inputMask = inputTiles$disturbanceYear, updateValue = 0,
                      inverseLogic = TRUE, mskVal = maskValue, spName = spName, x = x)
    return(jumbMasked)
    })
  
  names(distStack) <- paste0("year", times)
  
  #For each distance parameter, apply focal function (jumboFocal) to each year raster (distStack) using focalWeight matrices
  message(crayon::yellow(paste0("Applying focal operation to tile ", tileNumber, " of ", maxTile, " for ", spName)))
  
  fDistanceLists <- Map(inWeight = focalMatrices, denomRas = LCFocals, f = jumboFocal, MoreArgs = list(inList = distStack))
  
  names(fDistanceLists) <- paste("focal", focalDistance) # Unnecessary but left it in for browser transparency
  
  #Combine distance rasters by year
  newPlots <- MergeDistances(inList = fDistanceLists, 
                             times = times, 
                             birdDensityRasters = inputTiles$birdDensityRasters,
                             passedModel = passedModel,
                             intermPath = intermPath,
                             spName = spName)

  return(newPlots)
  
}
