applyFocalToTiles <- function(#useParallel = P(sim)$useParallel, # Should do paralell only for focal and predicting, maybe?
                                  listTilePaths = sim$listTilePaths,
                                  # pathData = dataPath(sim),
                                  # pathCache = cachePath(sim), 
                                  forestClass = P(sim)$forestClass,
                                  focalDistance = P(sim)$focalDistance,
                                  disturbanceClass = P(sim)$disturbanceClass,
                                  recoverTime = P(sim)$recoverTime,
                                  resampledRes = P(sim)$resampledRes,
                                  currentYear = time(sim)){
  
#  [ FIX ] Add parallel for tile masking!!!
  
# Subset matching tiles
  message(crayon::green("Tiles organized..."))
  totalTiles <- unique(lengths(listTilePaths))
  lengthVector <- 1:totalTiles
  orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
    sbset <- unlist(lapply(listTilePaths, `[[`, index), use.names = FALSE)
    return(sbset)
  })
  names(orderedRasterList) <- paste0("tile", lengthVector)
  
  message(crayon::red(paste0("Starting masking for year ",
                             currentYear, " (Time: "
                             , Sys.time(), ")")))
  
  # Entering each tile group
  focalTilesToMerge <- lapply(X = lengthVector, FUN = function(tiles){
      
# =============== RASTER 1 (in this case, disturbance TYPE raster) =============== #
    
             Raster1 <- raster::raster(orderedRasterList[[tiles]][1])
             Raster1[] <- Raster1[] %>% # Bring raster to memory, faster processing
               round(0)  # Round to 0, useful for integer rasters, makes them smaller
             storage.mode(Raster1[]) <- "integer" # Reducing size of raster by converting it to a real binary
             
      if (!all(is.na(Raster1[])) == TRUE) {

             # 1. Converting raster to binary to select only harvesting disturbances
             binaryRaster1 <- binaryReclassify(inFile = Raster1, inValues = disturbanceClass)
             rm(Raster1)
             gc()
             
             # binaryRaster1 = binaryDisturb
             binaryRaster1[] <- binaryRaster1[] %>% # Bring raster to memory, faster processing
               round(0)  # Round to 0, useful for integer rasters, makes them smaller
               storage.mode(binaryRaster1[]) <- "integer" # Reducing size of raster by converting it to a real binary
               
               if (max(binaryRaster1[]) == 0) {
                 message(crayon::yellow(paste0("Year ", currentYear, " for ", names(orderedRasterList)[tiles], 
                                               " was skipped as it doesn't have activities type ", disturbanceClass)))
                 Raster3 <- binaryRaster1
                 
                 # Resample raster to 250m
                 y <- raster::raster(res = c(resampledRes, resampledRes), 
                                     crs = raster::crs(Raster3),
                                     ext = extent(Raster3))
                 Raster3 <- raster::resample(x = Raster3, y = y, method = "ngb")
                 return(Raster3)
               }
               
# =============== RASTER 2 (in this case, LAND COVER raster) =============== #
               
               Raster2 <- raster::raster(orderedRasterList[[tiles]][2])
               Raster2[] <- Raster2[] %>% # Bring raster to memory, faster processing
                 round(0)  # Round to 0, useful for integer rasters, makes them smaller
               storage.mode(Raster2[]) <- "integer" # Reducing size of raster by converting it to a real binary
               
               # 1. Converting raster to binary to select only harvesting disturbances
               binaryRaster2 <- binaryReclassify(inFile = Raster2, inValues = forestClass)
               rm(Raster2)
               gc()
               
               binaryRaster2[] <- binaryRaster2[] %>% # Bring raster to memory, faster processing
                 round(0)  # Round to 0, useful for integer rasters, makes them smaller
               storage.mode(binaryRaster2[]) <- "integer" # Reducing size of raster by converting it to a real binary
               
               # 2. create focal matrix (for each distance parameter). 
               #if annulus = TRUE, it will use the largest and smallest distances to make the annulus (you should only have 2). One day we will adapt this to work with multiple buffers concurrently 
               if(length(focalDistance)> 1) {
                 #make inner matrix
                 inMat <-
                   raster::focalWeight(x = binaryRaster2, d = min(focalDistance))
                 outMat <-
                   raster::focalWeight(x = binaryRaster2, d = max(focalDistance))
                 #inverse the inner matrix
                 inMat[inMat == 0] <- 1
                 inMat[inMat < 1] <- 0
                 #Get dimensions for matrix
                 innerDim <- floor(dim(inMat)[1] / 2)
                 outerDim <- ceiling(dim(outMat)[1] / 2)
                 #Merge the two matrices
                 outMat[(outerDim - innerDim):(outerDim + innerDim), 
                        (outerDim - innerDim):(outerDim + innerDim)] <- inMat
                 #Recalculate the matrix value as 1/sum of non-zero values
                 outMat[outMat > 0] <- 1 / length(outMat[outMat > 0])
                 focalMatrices <- list(outMat)
               } else {
                 focalMatrices <-
                   lapply(focalDistance, FUN = focalWeight, x = binaryRaster2)
               }
               
               # 3. calculate focal statistics with each matrix
               LCFocals <- lapply(focalMatrices, FUN = raster::focal, x = binaryRaster2, na.rm = TRUE)
               rm(binaryRaster2) # Remove binaryRaster2 so we free memory for the next raster to be processed
               gc()

# =============== RASTER 3 (in this case, disturbance YEAR raster) =============== #
               
               Raster3 <- raster::raster(orderedRasterList[[tiles]][3])
               Raster3[] <- Raster3[] %>% # Bring raster to memory, faster processing
                 round(0)  # Round to 0, useful for integer rasters, makes them smaller
               storage.mode(Raster3[]) <- "integer" # Reducing size of raster by converting it to a real binary
               
               if (currentYear > 1000){
                 currentYear <- currentYear - 1900
               }
               
               # Calculate cummulative effects. If not wanted, change to maskValue <- currentYear
               maskValue <- c((currentYear - recoverTime) : currentYear)
               
               # Replace values of interest for big ones (999)
               yearValue <- getValues(Raster3)
               yearValue[yearValue %in% maskValue] <- 999
               Raster3 <- setValues(Raster3, yearValue)
               
               message(crayon::yellow(paste0("Masking ", names(orderedRasterList)[tiles],
                                             " of ", totalTiles, " tiles (Time: "
                                             , Sys.time(), ")")))
               
               Raster3 <- raster::mask(x = binaryRaster1, mask = Raster3, maskvalue = 999, inverse = TRUE, updatevalue = 0)
               
               # Resample raster to 250m
               y <- raster::raster(res = c(resampledRes, resampledRes), 
                                   crs = raster::crs(Raster3),
                                   ext = extent(Raster3))
               Raster3 <- raster::resample(x = Raster3, y = y, method = "ngb")
               
              return(Raster3)
  } else {
    message(crayon::red(paste0(tileNumber, 
                               " was skipped as it has only NA values (Time: ", Sys.time(), ")")))
    y <- raster::raster(res = c(resampledRes, resampledRes), 
                        crs = raster::crs(binaryRaster1),
                        ext = extent(binaryRaster1))
    Raster3 <- raster::resample(x = binaryRaster1, y = y, method = "ngb")
    return(binaryRaster1)
  }
  })

  gc()
  # IF FAILS (ALL 250m RASTERS ARE IN MEMORY, ADD filename ARGUMENT TO resample() so it writes to disk)
  # mergedFocalTiles <- SpaDES.tools::mergeRaster(focalTilesToMerge) # NOT SURE IT WORKS WITH RASTERS IN MEMORY UNCOMMENT [ FIX ]
  gc()
  
  mergedFocalTiles <- focalTilesToMerge[[1]]  # DUMMY LINE EXCLUDE [ FIX ]
  mergedFocalTiles@data@names <- paste0("Year", currentYear)
  
  return(mergedFocalTiles)
}

