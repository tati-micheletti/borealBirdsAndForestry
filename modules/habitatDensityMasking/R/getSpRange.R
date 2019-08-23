getSpRange <- function(species,
                       birdsRangeList,
                       pathHR = tempdir(),
                       rasterToMatch){
  library("raster")
  library("reproducible")
  if (length(species) == 1){
    if (length(birdsRangeList) == 1){
      spRangePath <- birdsRangeList
    } else {
      spRangePath <- birdsRangeList[[species]]
    }
    spRange <- reproducible::prepInputs(url = spRangePath, 
                                        targetFile = paste0(species, "_corecurrmeanBSI.asc"), # From Stralberg et al. 2015 Diversity and Distribution
                                        destinationPath = pathHR, fun = "raster::raster")
    raster::crs(spRange) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" #Had to "guess" the crs, its not in the original files!
    spRange <- reproducible::postProcess(spRange, rasterToMatch = rasterToMatch, destinationPath = pathHR, useCache = TRUE)
    vals <- raster::getValues(x = spRange)
    vals[vals < 1] <- NA # Values lower than 1 mean that the density is so low, it is most likely absent from the area. Confirm with D. Stralberg
    spRange <- raster::setValues(x = spRange, values = vals)
    return(spRange)
  } else {
    allSp <- lapply(X = species, FUN = function(sp){
      spRangePath <- birdsRangeList[[sp]]
      spRange <- reproducible::prepInputs(url = spRangePath, 
                            targetFile = paste0(sp, "_corecurrmeanBSI.asc"), # From Stralberg et al. 2015 Diversity and Distribution
                            destinationPath = pathHR, fun = "raster::raster")
      raster::crs(spRange) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" #Had to "guess" the crs, its not in the original files!
      spRange <- reproducible::postProcess(spRange, rasterToMatch = rasterToMatch, destinationPath = pathHR, useCache = TRUE)
      vals <- raster::getValues(x = spRange)
      vals[vals < 1] <- NA # Values lower than 1 mean that the density is so low, it is most likely absent from the area. Confirm with D. Stralberg
      spRange <- raster::setValues(x = spRange, values = vals)
      return(spRange)
    })
    names(allSp) <- species
    return(allSp)
  }
}

