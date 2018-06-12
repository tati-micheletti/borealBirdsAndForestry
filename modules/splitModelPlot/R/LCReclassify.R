LCReclassify <- function(inputTiles, sim, ...){
  #Assumed that abundance reflects landcover of non-forest as well. 
  #reclassify landcover into binary with 1 = forest
  binaryLC <- binaryReclassify(inFile = inputTiles$land, inValues = P(sim)$forestClass)
  
  #2. create focal matrix (for each distance parameter)
  focalMatrices <- lapply(P(sim)$focalDistance, FUN = focalWeight, x = binaryLC)
  
  #3. calculee focal statistics with each matrix
  LCFocals <- lapply(focalMatrices, FUN = focal, x = binaryLC, na.rm = TRUE)
  
  #Calculate binary version of disturbance type
  binaryDisturb <- binaryReclassify(inFile = inputTiles$distType, inValues = P(sim)$disturbanceClass)
  
  #Generate new raster for each year, where only values disturbed in a given year are 1
  times <- c(start(sim):end(sim))
  
  distStack <- lapply(times, FUN = jumboMask, inputRas = binaryDisturb, inputMask = inputTiles$distYear, updateValue = 0, inverseLogic = TRUE)
  names(distStack) <- paste("year", times)
  
  #For each distance parameter, apply focal function (jumboFocal) to each year raster (distStack) using focalWeight matrices
  
  fDistanceLists <- Map(inWeight = focalMatrices, denomRas = LCFocals, f = jumboFocal, MoreArgs = list(inList = distStack))
  
  names(fDistanceLists) <- paste("focal", P(sim)$focalDistance) #unnecessary but left it in for browser transparency
  
  #Combine distance rasters by year
  newPlots <- MergeDistances(fDistanceLists, times = times, abund = inputTiles$inputAbundances, ...)
  
  return(newPlots)
  
}
