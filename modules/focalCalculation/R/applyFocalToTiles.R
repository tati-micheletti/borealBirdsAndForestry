applyFocalToTiles <- function(#useParallel = P(sim)$useParallel, # Should do paralell only for focal and predicting, maybe?
  # We will use parallel only if when all is in memory, it leaves space in memory for dealing with more than 1 at a time
  listTilePaths = sim$rastersList,
  pathData = dataPath(sim),
  forestClass = P(sim)$forestClass,
  focalDistance = P(sim)$focalDistance,
  disturbanceClass = P(sim)$disturbanceClass,
  recoverTime = P(sim)$recoverTime,
  resampledRes = P(sim)$resampledRes,
  currentYear = time(sim)){
  
  #  [ FIX ] Add parallel for tile masking!!! # Parallel here in one machine might not be possible to use...  
  
  # Subset matching tiles
  message(crayon::green("Tiles organized..."))
  totalTiles <- unique(lengths(listTilePaths))
  lengthVector <- 1:totalTiles
  orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
    sbset <- unlist(lapply(listTilePaths, `[[`, index), use.names = FALSE)
    return(sbset)
  })
  names(orderedRasterList) <- paste0("tile", lengthVector)
  
  message(crayon::red(paste0("Starting focal operations for year ",
                             currentYear, " (Time: "
                             , Sys.time(), ")")))
  
  # Entering each tile group
  focalTilesToMerge <- lapply(X = lengthVector, FUN = function(tiles){
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ M A S K I N G ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    
    # =============== RASTER 1 (in this case, disturbance TYPE raster) =============== #
    
    Raster1 <- orderedRasterList[[tiles]][[1]]
    message(crayon::yellow(paste0("Bringing Raster 1 ", paste0("Tile ", tiles)," to memory for faster processing...")))
    Raster1[] <- Raster1[] %>% # Bring raster to memory, faster processing 
      round(0)  # Round to 0, useful for integer rasters, makes them smaller
    storage.mode(Raster1[]) <- "integer" # Reducing size of raster by converting it to a real binary
    gc()
    # Original raster is 1.7Gb, in memory it is 33.3Gb, after integer and gc(), back to 7.5Gb
    
    if (Raster1@data@max == 0 | all(is.na(Raster1[])) == TRUE) {
      
      message(crayon::red(paste0("Year ", currentYear, " for Tile ", tiles, 
                                 " was skipped as it doesn't have activities type ", disturbanceClass)))
      # Resample raster to 250m
      message(crayon::yellow(paste0("Resampling Raster 1 ", paste0("Tile ", tiles), " (Time: "
                                    , Sys.time(), ")")))
      y <- raster::raster(res = c(resampledRes, resampledRes), 
                          crs = raster::crs(Raster1),
                          ext = extent(Raster1))
      gc()
      Raster3 <- Cache(raster::resample, x = Raster1, y = y, method = "ngb", overwrite = TRUE,
                       filename = file.path(pathData, paste0("resampled/resampledSkipped", tiles)),
                       userTags = paste0("functionFinality:resampledSkipped", tiles))
      rm(Raster1)
      gc()      
      Raster3 <- raster::raster(file.path(pathData, paste0("resampled/resampledSkipped", tiles)))
      gc()
      return(Raster3)
      
    } else {
      
      # 1. Converting raster to binary to select only harvesting disturbances
      Raster1 <- Cache(binaryReclassify, inFile = Raster1, inValues = disturbanceClass, 
                       userTags = paste0("objectName:binaryRaster1", tiles))
      storage.mode(Raster1[]) <- "integer" # Reducing size of raster by converting it to a real binary
      gc()
      
      # =============== RASTER 2 (in this case, LAND COVER raster) =============== #
      
      Raster2 <- orderedRasterList[[tiles]][[2]]
      Raster2[] <- Raster2[] %>% # Bring raster to memory, faster processing
        round(0)  # Round to 0, useful for integer rasters, makes them smaller
      storage.mode(Raster2[]) <- "integer" # Reducing size of raster by converting it to a real binary
      gc()
      
      # 1. Converting raster to binary to select only harvesting disturbances
      Raster2 <- Cache(binaryReclassify, inFile = Raster2, inValues = forestClass, 
                       userTags = paste0("objectName:binaryRaster2", tiles))
      gc()
      storage.mode(Raster2[]) <- "integer" # Reducing size of raster by converting it to a real binary
      gc()
      
      # 2. create focal matrix (for each distance parameter). 
      if(length(focalDistance)> 1) {
        #if you pass two focalDistances, it will make an annulus (you should only have 2!).
        #make inner matrix
        gc()
        inMat <- raster::focalWeight(x = Raster2, d = min(focalDistance))
        gc()
        outMat <- raster::focalWeight(x = Raster2, d = max(focalDistance))
        #inverse the inner matrix
        inMat[inMat == 0] <- 1
        gc()
        inMat[inMat < 1] <- 0
        gc()
        #Get dimensions for matrix
        innerDim <- floor(dim(inMat)[1] / 2)
        gc()
        outerDim <- ceiling(dim(outMat)[1] / 2)
        gc()
        #Merge the two matrices
        outMat[(outerDim - innerDim):(outerDim + innerDim), 
               (outerDim - innerDim):(outerDim + innerDim)] <- inMat
        
        gc()
        #Recalculate the matrix value as 1/sum of non-zero values
        outMat[outMat > 0] <- 1 / length(outMat[outMat > 0])
        gc()
        focalMatrices <- outMat
      } else {
        gc()
        focalMatrices <- Cache(raster::focalWeight, x = Raster2, 
                               d = focalDistance, userTags = paste0("functionFinality:focalWeight", tiles))
        gc()
      }
      
      # 3. calculate focal statistics with each matrix
      LCFocals <- Cache(raster::focal, x = Raster2, w = focalMatrices, 
                        na.rm = TRUE, userTags = paste0("functionFinality:focalMatrix", tiles))      
      rm(Raster2) # Remove Raster2 so we free memory for the next raster to be processed
      gc()
      
      # =============== RASTER 3 (in this case, disturbance YEAR raster) =============== #
      
      Raster3 <- orderedRasterList[[tiles]][[3]]
      Raster3[] <- Raster3[] %>% # Bring raster to memory, faster processing
        round(0)  # Round to 0, useful for integer rasters, makes them smaller
      storage.mode(Raster3[]) <- "integer" # Reducing size of raster by converting it to a real binary
      if (currentYear > 1000){
        currentYear <- currentYear - 1900
      }
      
      # Calculate cummulative effects. If not wanted, change to maskValue <- currentYear
      maskValue <- c((currentYear - recoverTime) : currentYear)
      # Replace values of interest for big ones (999)
      yearValue <- raster::getValues(Raster3)
      yearValue[yearValue %in% maskValue] <- 999
      Raster3 <- raster::setValues(Raster3, yearValue)
      rm(yearValue)
      gc()
      message(crayon::yellow(paste0("Masking Tile ", tiles,
                                    " of ", totalTiles, " tiles (Time: "
                                    , Sys.time(), ")")))
      
      if (!identical(raster::extent(Raster3), raster::extent(Raster1))){
        if (raster::ncell(Raster1) < raster::ncell(Raster3)) {
          Raster3 <- raster::crop(x = Raster3, y = Raster1)
        }
        if (raster::ncell(Raster1) > raster::ncell(Raster3)) {
          Raster1 <- raster::crop(x = Raster1, y = Raster3)
        }
        
        raster::extent(Raster3) <- raster::alignExtent(extent = raster::extent(Raster3), 
                                                       object = Raster1, 
                                                       snap = "near")
      }

      Raster3 <- Cache(raster::mask, x = Raster1, mask = Raster3, 
                       maskvalue = 999, inverse = TRUE, updatevalue = 0, 
                       userTags = paste0("functionFinality:masked", tiles))
      rm(Raster1)
      gc()
      
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ F O C A L ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
      
      message(crayon::yellow(paste0("Applying focal operation to ", 
                                    names(orderedRasterList)[tiles], 
                                    " of ", totalTiles, " tiles (Time: "
                                    , Sys.time(), ")")))
      if (!identical(raster::extent(Raster3), raster::extent(LCFocals))){
        if (raster::ncell(LCFocals) < raster::ncell(Raster3)) {
          Raster3 <- raster::crop(x = Raster3, y = LCFocals)
        }
        if (raster::ncell(LCFocals) > raster::ncell(Raster3)) {
          LCFocals <- raster::crop(x = LCFocals, y = Raster3)
        }
        
        raster::extent(Raster3) <- raster::alignExtent(extent = raster::extent(Raster3), 
                                                       object = LCFocals, 
                                                       snap = "near")
      }
      # Apply focal
      Raster3 <- Cache(individualFocal, inList = Raster3, inWeight = focalMatrices, denomRas = LCFocals, 
                       userTags = paste0("individualFocal",tiles))
      Raster3@data@names <- paste0(names(orderedRasterList)[tiles], "Focal", max(focalDistance))
      # Some inf values returned from dividing by 0 (non-forest in LCC), so we devided by LCFocals. 
      # Now we need to make the values that are above 1, 1.
      aboveValue <- raster::getValues(Raster3)
      aboveValue[aboveValue > 1] <- 1
      Raster3 <- raster::setValues(Raster3, aboveValue)
      rm(aboveValue)
      rm(LCFocals)
      gc()

      # Resample raster to 250m
      message(crayon::green(paste0("Resampling Tile ", 
                                   tiles, 
                                   " (focal distance ", max(focalDistance), 
                                   ") to ", resampledRes, "m resolution (Time: "
                                   , Sys.time(), ")")))

      Raster3FilePathRes <- file.path(pathData, paste0("resampled/resampled",
                                                       tiles, ".tif"))
      if (Raster3@data@max == 0) {
        mth <- "near"
      } else {
        mth <- "bilinear"
      }

      raster::writeRaster(Raster3, filename = file.path(pathData, paste0("toResample",
                                                                         tiles, ".tif")),
                          format = "GTiff", overwrite = TRUE)
      Raster3FilePath <- file.path(pathData, paste0("toResample",
                                                    tiles, ".tif"))
      system(
        paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp "),
               "-s_srs \"", as.character(raster::crs(Raster3)), "\"",
               " -t_srs \"", as.character(raster::crs(Raster3)), "\"",
               " -multi ",
               "-wo NUM_THREADS=35 ",
               "-ot UInt16 ",
               "-overwrite ",
               "-tr ", paste(rep(resampledRes, 2), collapse = " "), " ",
               "-r ", paste0(mth, " "),
               Raster3FilePath, " ",
               Raster3FilePathRes),
        wait = TRUE)
      Raster3 <- raster::raster(Raster3FilePathRes)
      gc()
      
      Raster3 <- raster::writeRaster(Raster3, filename = file.path(pathData, 
                                                                   paste0("resampled/resampled",
                                                                                    tiles)),
                                     overwrite = TRUE)
      Raster3 <- raster::raster(file.path(pathData, 
                                          paste0("resampled/resampled",
                                                 tiles)))
      return(Raster3)
    }
  })
  gc()
  mergedFocalTiles <- SpaDES.tools::mergeRaster(focalTilesToMerge)
  rm(focalTilesToMerge)
  gc()
  mergedTilesName <- file.path(pathData, paste0("mergedFocal", currentYear, resampledRes, "m"))
  raster::writeRaster(x = mergedFocalTiles, filename = mergedTilesName, overwrite = TRUE)
  rm(mergedFocalTiles)
  gc()
  return(raster::raster(mergedTilesName))
}

