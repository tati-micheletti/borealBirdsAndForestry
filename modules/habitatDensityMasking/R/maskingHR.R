maskingHR <- function(trendRasters = NULL,
                      birdsRangeList,
                      pathData = NULL){
  if (is.null(pathData)) pathData <- tempdir()
  if (is.null(trendRasters)){
    message("trendRasters is NULL. Returning only range rasters")
    rangeRas <- lapply(names(birdsRangeList), FUN = function(species){
    spRange <- getSpRange(species = species,
                          birdsRangeList = birdsRangeList,
                          pathHR = pathData)
    names(spRange) <- species
    return(spRange)
    })
    names(rangeRas) <- names(birdsRangeList)
    return(rangeRas)
  } else {
  newTrendRas <- lapply(names(trendRasters), FUN = function(species){
    pathHR <- file.path(pathData, "maskedHR")
    suppressWarnings(dir.create(pathHR))
    rasNameSp <- file.path(pathHR, paste0(species, "TrendMaskedHR.tif"))
    if (file.exists(rasNameSp)) {
      return(raster::raster(rasNameSp))
    } else {
      spRange <- getSpRange(species = species,
                            birdsRangeList = birdsRangeList,
                            pathHR = pathHR)
      spRange <- projectInputs(x = spRange, rasterToMatch = trendRasters[[species]]) 
      # Need to reproject the SpRange as I want the same projection as the original trends raster in the end
      suppressWarnings(reproducible::postProcess(x = trendRasters[[species]], rasterToMatch = spRange, 
                               maskWithRTM = TRUE, format = "GTiff", 
                               filename2 = rasNameSp, overwrite = TRUE))
      return(raster::raster(rasNameSp))
    }
  })
  return(newTrendRas)
  }
}