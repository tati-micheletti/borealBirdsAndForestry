applyFocalToTiles <- function(listTilesDisturbance,
                              listTilesRTM,
                              listTilesLCC,
                              pathCache,
                              focalDistance,
                              recoverTime,
                              pathData,
                              year,
                              classesToExcludeInLCC){
  
  # If the file is cached, then get only the rasters!
  if ("cacheRaster" %in% names(listTilesDisturbance)){
    listTilesDisturbance <- listTilesDisturbance[[1]]
  }
  if ("cacheRaster" %in% names(listTilesRTM)){
    listTilesRTM <- listTilesRTM[[1]]
  }
  if ("cacheRaster" %in% names(listTilesLCC)){
    listTilesLCC <- listTilesLCC[[1]]
  }
  # Subset matching tiles
  listRasters <- list(RTM = listTilesRTM,
                      disturbance = listTilesDisturbance,
                      LCC = listTilesLCC)
  totalTiles <- unique(lengths(listRasters))
  lengthVector <- 1:totalTiles
  orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
    sbset <- unlist(lapply(listRasters, `[[`, index), use.names = FALSE)
    return(sbset)
  })
  message(crayon::green("Tiles organized..."))
  
  names(orderedRasterList) <- paste0("tile", lengthVector)
  
  message(crayon::red(paste0("Starting focal operations for year ", year," (Time: "
                             , Sys.time(), ")")))
  
  invisible(checkPath(file.path(pathData, "processed"), create = TRUE))
  resampledRes <- unique(res(orderedRasterList[[1]][[1]]))
  
    allFocalDistance <- lapply(focalDistance, function(FDISTANCE){

      mergedTilesName <- file.path(pathData, paste0("mergedFocal", year, "_",
                                                    max(FDISTANCE), "Res", resampledRes, "m.tif"))
      mergedTilesAvailableName <- file.path(pathData, paste0("mergedAvailablePixelsFocal_",
                                                    max(FDISTANCE), "Res", resampledRes, "m.tif"))
  if (all(file.exists(mergedTilesName), 
          file.exists(mergedTilesAvailableName))) {
    message(crayon::green(paste0("Year ",
                                 currentYear, " has already been processed and is being loaded.",
                                 " Skipping to next year...")))
    
    return(list(focalDisturbance = raster::raster(mergedTilesName),
                availablePixels = raster::raster(mergedTilesAvailableName)))
    
  } else {

      # Entering each tile group
      # focalTilesToMerge is a list of 2 elements: one
      focalTilesToMerge <- lapply(X = lengthVector, FUN = focalToTiles, 
                                  totalTiles = totalTiles,
                                  orderedRasterList = orderedRasterList,
                                  pathData = pathData,
                                  focalDistance = FDISTANCE,
                                  recoverTime = recoverTime,
                                  resampledRes = resampledRes,
                                  currentYear = year,
                                  classesToExcludeInLCC = classesToExcludeInLCC)
    gc()
    
    # Organize the tiles: all tiles of *availablePixels* and all tiles of *focalDisturbance*
    # It becomes one list of 2 elements (*availablePixels* and *focalDisturbance*), 
    # each one with all tiles
    browser() # Check about the names
    totalTiles <- unique(length(focalTilesToMerge))
    lengthVector <- 1:totalTiles
    orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
      sbset <- unlist(lapply(listRasters, `[[`, index), use.names = FALSE) # Maybe use.names = TRUE?
      return(sbset)
    })
    names(orderedRasterList) <- c("focalAvailablePixels", 
                                  "focalDisturbance")
    
      if (!file.exists(mergedTilesAvailableName)){
        focalTilesToMerge <- orderedRasterList[["focalAvailablePixels"]]
        rasMosaicArgs <- focalTilesToMerge
        rasMosaicArgs$fun <- max
        mergedFocalTiles <- do.call(what = raster::mosaic, args = rasMosaicArgs)
        rm(focalTilesToMerge)
        rm(rasMosaicArgs)
        gc()
        mergedFocalTiles <- round(x = mergedFocalTiles, digits = 4)
        raster::writeRaster(x = mergedFocalTiles, filename = mergedTilesName, 
                            overwrite = TRUE, format = "GTiff")
        rm(mergedFocalTiles)
        gc()
      }
      if (!file.exists(mergedTilesAvailableName)){
        focalTilesToMerge <- orderedRasterList[["focalDisturbance"]]
        rasMosaicArgs <- focalTilesToMerge
        rasMosaicArgs$fun <- max
        mergedFocalTiles <- do.call(what = raster::mosaic, args = rasMosaicArgs)
        rm(focalTilesToMerge)
        rm(rasMosaicArgs)
        gc()
        mergedFocalTiles <- round(x = mergedFocalTiles, digits = 4)
        raster::writeRaster(x = mergedFocalTiles, filename = mergedTilesName, 
                            overwrite = TRUE, format = "GTiff")
        rm(mergedFocalTiles)
        gc()
      }
    
    return(list(focalDisturbance = raster::raster(mergedTilesName),
                availablePixels = raster::raster(mergedTilesAvailableName)))
  } # End of ifelse for checking for the full merged file
  
  })
}