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
  
  #split abundance separately as Float
  birdDensityRasters <- splitRaster(r = birdDensityRasters, nx = nx, ny = ny, buffer = buffer, rType = "FLT4S")
  
  newlist <- Cache(Map, rasterList, path = file.path(pathData, names(rasterList)), f =  splitRaster,
                   MoreArgs = list(nx = nx, ny = ny, buffer = buffer, rType = rType))
  newlist[["birdDensityRasters"]] <- birdDensityRasters
  
  origNames <- names(rasterList)
  origNames[4] <- "birdDensityRasters"
  
  lengthvect <- 1:(nx * ny)
  outList <- lapply(lengthvect, FUN = tileReorder, 
                    inList = newlist, 
                    origList = origNames, 
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
    browser()
    rasList <- lapply(X = outList, `[[`, nRas)
    rasName <- names(outList[[1]])[nRas]
    dir.create(file.path(intermPath, "outputRasters"), showWarnings = FALSE)
    finalRasPath <- file.path(intermPath, "outputRasters", paste0(rasName, ".tif"))

    # IF USING gdalUtils::mosaic_rasters()
      # Define a directory for the intermediate rasters, as gdalUtils needs them from disk
    # dir.create(file.path(intermPath, "intermediateRasters"), showWarnings = FALSE)
    # intermPathFull <- file.path(intermPath, "intermediateRasters")
    # tempRasNames <- lapply(X = rasList, FUN = function(x){
    #   # Write to disk tiles to be put together
    #   tmpFile <- tempfile(pattern = rasName, tmpdir = intermPathFull, fileext = ".tif")
    #   writeRaster(x = x, filename = tmpFile)
    #   return(tmpFile)
    # })
    
    # gdalUtils::mosaic_rasters(gdalfile = unlist(tempRasNames),
    #                             dst_dataset = finalRasPath)
    
    # rm(rasName) # Remove raster from memory
    # gc() # Clean their memory # Check the package for arguments. Don't forget to parelell: idea: paralell the tiles' lapply.  
    # return(finalRasPath)
      
  })
  
  names(finalRasPath) <- names(outList[[1]])
  return(finalRasList)
  
}
