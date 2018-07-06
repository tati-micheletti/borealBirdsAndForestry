LCReclassify <- function(inputTiles = tilelist, 
                         startTime = startTime,
                         endTime = endTime,
                         forestClass = forestClass,
                         focalDistance = focalDistance,
                         disturbanceClass = disturbanceClass,
                         pathData = pathData, 
                         intermPath = intermPath,
                         spName = spName,
                         passedModel = passedModel){

  #Assumed that abundance reflects landcover of non-forest as well. 
  #reclassify landcover into binary with 1 = forest
  message(crayon::yellow(paste0("Processing tiles for ", spName)))
  binaryLC <- binaryReclassify(inFile = inputTiles$land, inValues = forestClass)
  
  #2. create focal matrix (for each distance parameter)
  focalMatrices <- lapply(focalDistance, FUN = focalWeight, x = binaryLC)
  
  #3. calculee focal statistics with each matrix
  LCFocals <- lapply(focalMatrices, FUN = focal, x = binaryLC, na.rm = TRUE)
  
  #Calculate binary version of disturbance type
  binaryDisturb <- binaryReclassify(inFile = inputTiles$disturbanceType, inValues = disturbanceClass)
  
  #Generate new raster for each year, where only values disturbed in a given year are 1
  times <- c(startTime:endTime)
  
  browser()
  message(crayon::yellow(paste0("Masking tiles per year for ", spName)))
  distStack <- lapply(X = times, FUN = function(x){
    jumbMasked <- jumboMask(inputRas = binaryDisturb, 
                      inputMask = inputTiles$disturbanceYear, updateValue = 0, 
                      inverseLogic = TRUE, mskVal = x)
    return(jumbMasked)
    })
  
  names(distStack) <- paste("year", times)
  
  #For each distance parameter, apply focal function (jumboFocal) to each year raster (distStack) using focalWeight matrices
  message(crayon::yellow(paste0("Applying focal operation to tile for ", spName)))
  fDistanceLists <- Map(inWeight = focalMatrices, denomRas = LCFocals, f = jumboFocal, MoreArgs = list(inList = distStack))
  
  names(fDistanceLists) <- paste("focal", focalDistance) #unnecessary but left it in for browser transparency
  
  #Combine distance rasters by year
  newPlots <- MergeDistances(inList = fDistanceLists, 
                             times = times, 
                             birdDensityRasters = inputTiles$birdDensityRasters,
                             passedModel = passedModel,
                             intermPath = intermPath,
                             spName = spName)

  return(newPlots)
  
}
