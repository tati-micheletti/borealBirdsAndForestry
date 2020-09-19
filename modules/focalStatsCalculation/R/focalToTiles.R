focalToTiles <- function(tiles,
                         totalTiles,
                         orderedRasterList,
                         pathData,
                         focalDistance,
                         recoverTime,
                         resampledRes,
                         currentYear,
                         classesToExcludeInLCC){
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ T H E    R T M  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
  # 
  #  As the RTM that will generate the available pixels is the same for each year,
  # but the disturbance is NOT, maybe I can calculate the RTM focal separately! This would def
  # save time with the focal!!
  doRTM <- TRUE
  
  finalRasFilePathAvailable <- file.path(pathData, paste0("processed/availablePixelsTile",
                                                          tiles, "Res", resampledRes, "m_Focal", 
                                                          max(focalDistance),"m.tif"))
  
  if (file.exists(finalRasFilePathAvailable)){
    message(crayon::yellow(paste0("RTM raster from ", paste0("Tile ", tiles),
                                  " already exists. Saving time by skipping...")))
    doRTM <- FALSE
  }
  
  # Testing for final raster. If it exists, return. Otherwise, create
  finalRasFilePath <- file.path(pathData, paste0("processed/tile",
                                                 tiles, "Year", currentYear,
                                                 "Res", resampledRes, "m_Focal",
                                                 max(focalDistance),"m.tif"))
  if (all(file.exists(finalRasFilePath), !doRTM)){
    disturbanceRas <- raster::raster(finalRasFilePath)
    gc()
    message(crayon::yellow(paste0("Raster from ", paste0("Tile ", tiles),
                                  " for year ", currentYear,
                                  " already exists. Returning saved object...")))
    focalAvailablePixels <- raster::raster(finalRasFilePathAvailable)
    focalDisturbance <- raster::raster(finalRasFilePath)
    return(list("focalAvailablePixels" = focalAvailablePixels,
                "focalDisturbance" = focalDisturbance))
  } else {
    
    # Sanity check: we can't have the disturbance done and not have the RTM
    if (all(file.exists(finalRasFilePath), doRTM))
      stop("The RTM doesn't exist, but ", finalRasFilePath, 
           " does. Something went wrong. Debug focalToTiles.R function")
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ M A S K I N G ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
    # 
    #  Need to mask the NA's to zeros with the RTM. The disturbance raster 
    #  has NA's instead of zeros where forestry does not occur, not only 
    #  outside of the study area, which will mess up the focal calculation
    #  if not accounted for.
    
    if (doRTM){
      RTM <- orderedRasterList[[tiles]][[1]]
    } else {
      RTM <- raster::raster(finalRasFilePathAvailable)
    }
    message(crayon::yellow(paste0("Bringing RTM and disturbance rasters for ", 
                                  paste0("tile ", tiles),
                                  " to memory for faster processing...")))
    RTM[] <- RTM[] %>% # Bring raster to memory, faster processing 
      round(0)  # Round to 0, useful for integer rasters, makes them smaller
    storage.mode(RTM[]) <- "integer" # Reducing size of raster by converting it to a real binary
    gc()
    
    if (any(RTM@data@max == 0, all(is.na(RTM[])) == TRUE)) {
      
      message(crayon::red(paste0("Year ", currentYear, " for Tile ", tiles, 
                                 " was skipped as it doesn't have data")))

      return(list("focalAvailablePixels" = orderedRasterList[[tiles]][[1]],
                  "focalDisturbance" = orderedRasterList[[tiles]][[2]]))
    }
      
      # 4. create focal matrix (for each distance parameter). 
      if(length(focalDistance)> 1) {
        #if you pass two focalDistances, it will make an annulus (you should only have 2!).
        # make inner matrix
        focalMatrices <- Cache(focalWindowAnnulus, 
                               focalDistance = focalDistance, 
                               ras = RTM,
                               cacheId = "focalWindowAnnulus")
      } else {
        focalMatrices <- Cache(focalWindow, 
                               x = RTM, 
                               d = focalDistance,
                               cacheId = "focalWindow")
      }
      
      if (doRTM){
        # 0. Here we need to mask the RTM with the classes that have no available habitat to be 0 
        # (water bodies, ice, rocks, humans) because the disturbance focal will convert the 0's 
        # from this layer into NA's in the disturbance layer
        LCC <- orderedRasterList[[tiles]][[3]]
        LCC[] <- LCC[] %>% # Bring raster to memory, faster processing 
          round(0)  # Round to 0, useful for integer rasters, makes them smaller
        storage.mode(LCC[]) <- "integer" # Reducing size of raster by converting it to a real binary
        gc()
        RTM[LCC[] %in% classesToExcludeInLCC] <- 0
        rm(LCC) # Remove RTM, don't need it anymore
        gc()
        
        message(crayon::yellow(paste0("Applying focal operation on available pixels for Tile ", 
                                      tiles, " of ", totalTiles, " tiles (Time: "
                                      , Sys.time(), ")")))
      
        RTM <- individualFocal(ras = RTM,
                               weightMatrix = focalMatrices,
                               useInteger = TRUE,
                               maskTo = c(0, 0))
        
        # Writing available pixels' raster
        RTM@data@names <- paste0("availablePixelsTile", tiles,
                                 "Focal", max(focalDistance))
        writeRaster(RTM, filename = finalRasFilePathAvailable, 
                    format = "GTiff",
                    datatype = "INT2U")
      }
      
      # 1. Correct the NA's in disturbance raster
      disturbanceRaster <- orderedRasterList[[tiles]][[2]]
      disturbanceRaster[] <- disturbanceRaster[] %>% # Bring raster to memory, faster processing 
        round(0)  # Round to 0, useful for integer rasters, makes them smaller
      storage.mode(disturbanceRaster[]) <- "integer" # Reducing size of raster by converting it to a real binary
      gc()
      disturbanceRaster[RTM == 0] <- NA
      gc()
      
      # 2. Select which years to mask for
      if (currentYear > 1000){
        currentYear <- currentYear - 1900
      }

      maskValue <- c((currentYear - recoverTime) : currentYear)
      
      # Replace values of interest for big ones (999)
            message(crayon::yellow(paste0("Masking Tile ", tiles,
                                          " of ", totalTiles, " tiles (Time: "
                                          , Sys.time(), ")")))

      disturbanceRaster[disturbanceRaster[] %in% maskValue] <- 999
      
      # 3. Converting raster to binary to select only disturbances of the specific year
      disturbanceRaster[disturbanceRaster[] != 999] <- 0
      disturbanceRaster[disturbanceRaster[] == 999] <- 1
      
      storage.mode(disturbanceRaster[]) <- "integer" # Reducing size of raster by converting it to a real binary
      gc()
      
      # =============== F O C A L   C A L C U L A T I O N S =============== #
      
      # 3. calculate focal statistics with each matrix
      message(crayon::yellow(paste0("Applying focal operation to Tile ", 
                                    tiles, " of ", totalTiles, " tiles (Time: "
                                    , Sys.time(), ")")))

      # Apply focal
      disturbanceRaster <- individualFocal(ras = disturbanceRaster,
                                           weightMatrix = focalMatrices,
                                           denominatorRaster = RTM,
                                           maskTo = c(NA, NA))
      rm(RTM) # Remove RTM, don't need it anymore
      gc()

      # Writing disturbance raster
      disturbanceRaster[["focalDisturbance"]]@data@names <- paste0("focalDisturbanceTile", tiles,
                                                                   "Focal", max(focalDistance))
      writeRaster(disturbanceRaster[["focalDisturbance"]], 
                  filename = finalRasFilePath, 
                  format = "GTiff",
                  datatype = "FLT4S")

      focalAvailablePixels <- raster::raster(finalRasFilePathAvailable)
      gc()
      focalDisturbance <- raster::raster(finalRasFilePath)
      gc()
      return(list("focalAvailablePixels" = focalAvailablePixels,
                  "focalDisturbance" = focalDisturbance))
  }
}