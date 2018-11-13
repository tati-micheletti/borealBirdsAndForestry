applyFocalToTiles <- function(#useParallel = P(sim)$useParallel, # Should do paralell only for focal and predicting, maybe?
  # We will use parallel only if when all is in memory, it leaves space in memory for dealing with more than 1 at a time
  listTilePaths = sim$rastersList,
  pathData = cachePath(sim),
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
  suppressWarnings(dir.create(file.path(pathData, "focalCalculation", "resampled")))
  mergedTilesName <- file.path(pathData, "focalCalculation", paste0("mergedFocal", currentYear, "-", max(focalDistance), "Res", resampledRes, "m.tif"))
  if (file.exists(mergedTilesName)) {
    message(crayon::green(paste0("Year ",
                               currentYear, " has already been processed and is being loaded.",
                               " Skipping to next year...")))
    return(raster::raster(mergedTilesName))
  } else {
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
                         filename = file.path(pathData, "focalCalculation", paste0("resampled/resampledSkipped", tiles)),
                         cacheId = paste0("resampledSkipped", tiles))
        rm(Raster1)
        gc()
        # So that the raster is not in memory anymore
        Raster3 <- raster::raster(file.path(pathData, "focalCalculation", paste0("resampled/resampledSkipped", tiles)))
        gc()
        return(Raster3)
        
      } else {
        
        # 1. Converting raster to binary to select only harvesting disturbances
        Raster1 <- binaryReclassify(inFile = Raster1, inValues = disturbanceClass)
        storage.mode(Raster1[]) <- "integer" # Reducing size of raster by converting it to a real binary
        gc()
        
        # =============== RASTER 2 (in this case, LAND COVER raster) =============== #
        
        Raster2 <- orderedRasterList[[tiles]][[2]]
        Raster2[] <- Raster2[] %>% # Bring raster to memory, faster processing
          round(0)  # Round to 0, useful for integer rasters, makes them smaller
        storage.mode(Raster2[]) <- "integer" # Reducing size of raster by converting it to a real binary
        gc()
        
        # 1. Converting raster to binary to select only harvesting disturbances
        Raster2 <- binaryReclassify(inFile = Raster2, inValues = forestClass)
        gc()
        storage.mode(Raster2[]) <- "integer" # Reducing size of raster by converting it to a real binary
        gc()
        
        # 2. create focal matrix (for each distance parameter). 
        if(length(focalDistance)> 1) {
          #if you pass two focalDistances, it will make an annulus (you should only have 2!).
          #make inner matrix
          focalMatrices <- Cache(focalWeightAnnulus, focalDistance = focalDistance,
                                 cacheId = paste0("focalWeightAnnulus", tiles))
        } else {
          gc()
          focalMatrices <- Cache(raster::focalWeight, x = Raster2, 
                                 d = focalDistance, cacheId = paste0("focalWeight", tiles))
          gc()
        }
        
        # 3. calculate focal statistics with each matrix
        LCFocals <- raster::focal(x = Raster2, w = focalMatrices, 
                          na.rm = TRUE)    
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
        
        Raster3 <-raster::mask(x = Raster1, mask = Raster3, 
                         maskvalue = 999, inverse = TRUE, updatevalue = 0)
        rm(Raster1)
        gc()
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ F O C A L ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
        
        message(crayon::yellow(paste0("Applying focal operation to Tile ", 
                                      tiles, " of ", totalTiles, " tiles (Time: "
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
        Raster3 <- individualFocal(inList = Raster3, inWeight = focalMatrices, denomRas = LCFocals)
          
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
        
        Raster3FilePathRes <- file.path(pathData, "focalCalculation", paste0("resampled/resampled",
                                                                             tiles, "Year", currentYear, ".tif"))
        if (!file.exists(Raster3FilePathRes)){
          if (Raster3@data@max == 0) {
            mth <- "near"
          } else {
            mth <- "bilinear"
          }
          Raster3FilePath <- file.path(pathData, "focalCalculation", 
                                       paste0("toResample",
                                              tiles, "Year", currentYear, ".tif"))
          raster::writeRaster(Raster3, filename = Raster3FilePath, 
                              format = "GTiff", overwrite = TRUE)
          system(
            paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp "),
                   "-s_srs \"", as.character(raster::crs(Raster3)), "\"",
                   " -t_srs \"", as.character(raster::crs(Raster3)), "\"",
                   " -multi ",
                   "-wo NUM_THREADS=35 ",
                   "-ot Float32 ",
                   "-overwrite ",
                   "-tr ", paste(rep(resampledRes, 2), collapse = " "), " ",
                   "-r ", paste0(mth, " "),
                   Raster3FilePath, " ",
                   Raster3FilePathRes),
            wait = TRUE)
        }
        Raster3 <- raster::raster(Raster3FilePathRes)
        gc()
        return(Raster3)
      }
    })
    gc()
    mergedFocalTiles <- SpaDES.tools::mergeRaster(focalTilesToMerge)
    rm(focalTilesToMerge)
    gc()
    raster::writeRaster(x = mergedFocalTiles, filename = mergedTilesName, overwrite = TRUE, format = "GTiff")
    rm(mergedFocalTiles)
    gc()
    return(raster::raster(mergedTilesName))
  }
}