groupSplitRaster <- function(models = sim$models, 
                             birdDensityRasters = sim$birdDensityRasters,
                             disturbanceType = sim$disturbanceType,
                             disturbanceYear = sim$disturbanceYear,
                             landCover = sim$landCover,
                             pathData = pathData,
                             nx = P(sim)$nx,
                             ny = P(sim)$ny,
                             buffer = P(sim)$buffer,
                             rType = P(sim)$rType,
                             start = start(sim),
                             end = end(sim),
                             forestClass = P(sim)$forestClass,
                             focalDistance = P(sim)$focalDistance,
                             disturbanceClass = P(sim)$disturbanceClass) {
  

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
                    pathData = pathData)
  
  lengthResultRasters <- 1:length(outList[[1]])
  finalRasList <- lapply(lengthResultRasters, function(nRas){
    rasList <- lapply(X = outList, `[[`, nRas)
    rasName <- names(outList[[1]])[nRas]
    finalRasPath <- file.path("/mnt/storage/", "outputRasters", paste0(rasName, ".tif"))
    gdalUtils::mosaic_rasters(gdalfile = unlist(rasList), 
                                dst_dataset = finalRasPath)
      
    return(finalRasPath)
      
  })
  
  browser()
  
  names(finalRasPath) <- names(outList[[1]])
  return(finalRasList)
  
}
