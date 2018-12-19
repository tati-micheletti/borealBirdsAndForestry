maskingHR <- function(trendRasters = sim$trends,
                      birdsRangeList = sim$birdsRangeList,
                      pathData = cachePath(sim)){
  newTrendRas <- lapply(names(trendRasters), FUN = function(species){
    pathHR <- file.path(pathData, "maskedHR")
    suppressWarnings(dir.create(pathHR))
    rasNameSp <- file.path(pathHR, paste0(species, "TrendMaskedHR.tif"))
    if (file.exists(rasNameSp)) {
      return(raster::raster(rasNameSp))
    } else {
      spRangePath <- birdsRangeList[[species]]
      spRange <- prepInputs(url = spRangePath, 
                            targetFile = paste0(species, "_corecurrmeanBSI.asc"),
                            destinationPath = pathHR, fun = raster)
      crs(spRange) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" #Had to "guess" the crs, its not in the original files!
      vals <- raster::getValues(x = spRange)
      vals[vals < 1] <- NA
      spRange <- raster::setValues(x = spRange, values = vals)
      spRange <- projectInputs(x = spRange, rasterToMatch = trendRasters[[species]]) # Need to reproject the SpRange as I want the same projection as the original trends raster in the end
      suppressWarnings(reproducible::postProcess(x = trendRasters[[species]], rasterToMatch = spRange, 
                               maskWithRTM = TRUE, format = "GTiff", 
                               filename2 = rasNameSp, overwrite = TRUE))
      return(raster::raster(rasNameSp))
    }
  })
  return(newTrendRas)
}