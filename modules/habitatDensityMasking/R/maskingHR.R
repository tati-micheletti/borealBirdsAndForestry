maskingHR <- function(trendRasters = sim$trends,
                      birdsRangeList = sim$birdsRangeList,
                      pathData = cachePath(sim)){
  newTrendRas <- lapply(names(trendRasters), FUN = function(species){
    suppressWarnings(dir.create(pathHR))
    pathHR <- file.path(pathData, "maskedHR")
    rasNameSp <- file.path(pathHR, paste0(species, "TrendMaskedHR.tif"))
    if (file.exists(rasNameSp)) {
      return(raster::raster(rasNameSp))
    } else {
      spRangePath <- birdsRangeList[[species]]
      spRange <- preProcess(url = spRangePath, destinationPath = pathHR)
      reproducible::maskInputs(x = trendRasters[[species]], rasterToMatch = spRange, 
                               maskWithRTM = TRUE, format = "GTiff", 
                               filename2 = rasNameSp)
      return(raster::raster(rasNameSp))
    }
  })
  return(newTrendRas)
}