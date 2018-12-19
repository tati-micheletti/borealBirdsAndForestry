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
  
  #2. create focal matrix (for each distance parameter). 
  #if annulus = TRUE, it will use the largest and smallest distances to make the annulus (you should only have 2). One day we will adapt this to work with multiple buffers concurrently 
  if(length(focalDistance)> 1){
    #make inner matrix
    inMat <- raster::focalWeight(x = binaryLC, d = min(focalDistance))
    outMat <- raster::focalWeight(x = binaryLC, d = max(focalDistance))
    #inverse the inner matrix
    inMat[inMat == 0] <- 1
    inMat[inMat < 1] <- 0
    #Get dimensions for matrix
    innerDim <- floor(dim(inMat)[1]/2)
    outerDim <- ceiling(dim(outMat)[1]/2)
    #Merge the two matrices
    outMat[(outerDim-innerDim):(outerDim+innerDim),(outerDim-innerDim):(outerDim+innerDim)] <- inMat
    #Recalculate the matrix value as 1/sum of non-zero values
    outMat[outMat>0] <- 1/length(outMat[outMat>0])
    focalMatrices <- list(outMat)
  }else{
    focalMatrices <- lapply(focalDistance, FUN = focalWeight, x = binaryLC)
  }
  #3. calculee focal statistics with each matrix
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
  
  names(fDistanceLists) <- paste("focal", max(focalDistance)) # Unnecessary but left it in for browser transparency
  
  browser() # Should I resample the tiles here? If the rasters are rasters here already, then resample, if paths, resample in next ()
  #Combine distance rasters by year
  newPlots <- MergeDistances(inList = fDistanceLists, 
                             times = times, 
                             birdDensityRasters = inputTiles$birdDensityRasters,
                             passedModel = passedModel,
                             intermPath = intermPath,
                             spName = spName)

  return(newPlots)
  
}
