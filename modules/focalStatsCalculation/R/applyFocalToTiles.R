applyFocalToTiles <- function(listTilesDisturbance,
                              listTilesRTM,
                              pathCache,
                              focalDistance,
                              recoverTime,
                              pathData,
                              years){
  
  # If the file is cached, then get only the rasters!
  if ("cacheRaster" %in% names(listTilesDisturbance)){
    listTilesDisturbance <- listTilesDisturbance[[1]]
  }
  if ("cacheRaster" %in% names(listTilesRTM)){
    listTilesRTM <- listTilesRTM[[1]]
  }
  # Subset matching tiles
  listRasters <- list(RTM = listTilesRTM,
                      disturbance = listTilesDisturbance)
  totalTiles <- unique(lengths(listRasters))
  lengthVector <- 1:totalTiles
  orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
    sbset <- unlist(lapply(listRasters, `[[`, index), use.names = FALSE)
    return(sbset)
  })
  message(crayon::green("Tiles organized..."))
  
  names(orderedRasterList) <- paste0("tile", lengthVector)
  
  message(crayon::red(paste0("Starting focal operations (Time: "
                             , Sys.time(), ")")))
  
  invisible(checkPath(file.path(pathData, "processed"), create = TRUE))
  resampledRes <- unique(res(orderedRasterList[[1]][[1]]))
  
  allYears <- lapply(years, function(YEAR){
    allFocalDistance <- lapply(focalDistance, function(FDISTANCE){

      mergedTilesName <- file.path(pathData, paste0("mergedFocal", YEAR, "-",
                                                    max(FDISTANCE), "Res", resampledRes, "m.tif"))
  if (file.exists(mergedTilesName)) {
    message(crayon::green(paste0("Year ",
                                 currentYear, " has already been processed and is being loaded.",
                                 " Skipping to next year...")))
    
    return(raster::raster(mergedTilesName))
    
  } else {    
              # I removed parallel because there is no point in doing parallel. 
              # The whole reason why we divide it into tiles is to be able 
              # to work with memory, and if we max our possibility here, we
              # don't have RAM to paralellize. To use parallel across 
              # machines is also a challange as the tiles are huge. 
              # Overhead is not worth it, I believe. ~TM SEP20

      browser()
      # Entering each tile group
      focalTilesToMerge <- lapply(X = lengthVector, FUN = focalToTiles, 
                                  totalTiles = totalTiles,
                                  orderedRasterList = orderedRasterList,
                                  pathData = pathData,
                                  focalDistance = FDISTANCE,
                                  recoverTime = recoverTime,
                                  resampledRes = resampledRes,
                                  currentYear = YEAR)
      
    gc()
    focalTilesToMerge <- lapply(X = focalTilesToMerge, FUN = function(ras){
      rTemplate <- focalTilesToMerge[[1]]
      raster::extent(ras) <- raster::alignExtent(extent = raster::extent(ras),
                                                 object = rTemplate,
                                                 snap = "near")
      return(ras)
    })
    rasMosaicArgs <- focalTilesToMerge
    rasMosaicArgs$fun <- max
    mergedFocalTiles <- do.call(what = raster::mosaic, args = rasMosaicArgs)
    rm(focalTilesToMerge)
    rm(rasMosaicArgs)
    gc()
    mergedFocalTiles <- round(x = mergedFocalTiles, digits = 4)
    raster::writeRaster(x = mergedFocalTiles, filename = mergedTilesName, overwrite = TRUE, format = "GTiff")
    rm(mergedFocalTiles)
    gc()
    return(raster::raster(mergedTilesName))
  } # End of ifelse for checking for the full merged file
  
    })
    
  })
}