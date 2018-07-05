groupSplitRaster <- function(models = models, # This is already lapplying though each species
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
                             intermPath = intermPath,
                             rP = rP) {
  
  rasterList <- list("distType" = disturbanceType, "distYear" = disturbanceYear, "land" = landCover) # Original rasters' list
  newlist <- Cache(Map, rasterList, path = file.path(pathData, names(rasterList)), f =  splitRaster, # Splitted rasters' list
                   MoreArgs = list(nx = nx, ny = ny, buffer = buffer, rType = rType))
  
  #split abundance separately as Float
  # HERE: Resample to 30m, mask to sA, and then split and then delete (rm and gc).

    if (!all(raster::res(birdDensityRasters) == c(30, 30))){
      templateRaster <- raster(resolution = c(30, 30), 
                               crs = raster::crs(birdDensityRasters), 
                               ext = extent(birdDensityRasters))
      birdDensityRasters <- raster::resample(birdDensityRasters, templateRaster, method = "bilinear")
    }
  
  birdDensityRasters[] <- birdDensityRasters[]*1000
  birdDensityRasters <- splitRaster(r = birdDensityRasters, nx = nx, ny = ny, buffer = buffer, rType = "INT2S") # Splitting abundance Raster

  # Add abundance rasters to list
  newlist[["birdDensityRasters"]] <- birdDensityRasters
  
  # Get rasters Names
  origNames <- c(names(rasterList), "birdDensityRasters") # Fixed names. Were adding separately before
  
  # Remove old big rasters from memory
  # rm(rasterList, disturbanceType, disturbanceYear, landCover, birdDensityRasters) 
  #This is only removing the pointers... [ FIX ] once someone answers the question

  lengthvect <- 1:(nx * ny)
  
  # Outlist is a list of all the tiles after running the predictions
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
    
    # rm(get(rasName)) # Remove raster from memory
    # gc() # Clean their memory # Check the package for arguments. Don't forget to parelell: idea: paralell the tiles' lapply.  
    # return(finalRasPath)
      
  })
  
  browser()
  
  names(finalRasPath) <- names(outList[[1]])
  return(finalRasList)
  
}
