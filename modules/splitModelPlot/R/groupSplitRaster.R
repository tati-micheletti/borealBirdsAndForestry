groupSplitRaster <- function(models = models,
                             birdDensityRasters = birdDensityRasters,
                             disturbanceType = disturbanceType,
                             disturbanceYear = disturbanceYear,
                             landCover = landCover,
                             pathData = pathData,
                             nx = nx,
                             ny = ny,
                             buffer = buffer,
                             rType = rType,
                             start = start,
                             end = end,
                             forestClass = forestClass,
                             focalDistance = focalDistance,
                             disturbanceClass = disturbanceClass,
                             intermPath = intermPath) {
  
  rasterList <- list("distType" = disturbanceType, "distYear" = disturbanceYear, "land" = landCover) 
  rasterList[["birdDensityRasters"]] <- birdDensityRasters
  newlist <- Cache(Map, rasterList, path = file.path(pathData, names(rasterList)), f =  splitRaster,
                   MoreArgs = list(nx = nx, ny = ny, buffer = buffer, rType = rType))
  
  lengthvect <- 1:(nx * ny)
  outList <- lapply(lengthvect, FUN = tileReorder, 
                    inList = newlist, 
                    origList = rasterList, 
                    start = start,
                    end = end,
                    forestClass = forestClass,
                    focalDistance = focalDistance,
                    disturbanceClass = disturbanceClass, 
                    passedModel = models,
                    pathData = pathData,
                    intermPath = intermPath)
  
  lengthResultRasters <- 1:length(outList[[1]])
  finalRasList <- lapply(lengthResultRasters, function(nRas){
    rasList <- lapply(X = outList, `[[`, nRas)
    rasName <- names(outList[[1]])[nRas]
    browser()
    dir.create(file.path(intermPath, "outputRasters"), showWarnings = FALSE)
    finalRasPath <- file.path(intermPath, "outputRasters", paste0(rasName, ".tif"))
    tempTilesPath <- file.path("/mnt/storage/", "borealBirdsAndForestry", paste0(rasName))
    gdalUtils::mosaic_rasters(gdalfile = unlist(rasList),
                                dst_dataset = finalRasPath)
      
    return(finalRasPath)
      
  })
  
  browser()
  
  names(finalRasPath) <- names(outList[[1]])
  return(finalRasList)
  
}
