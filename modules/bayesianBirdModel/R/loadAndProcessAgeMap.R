loadAndProcessAgeMap <- function(dataPath = dataPath(sim), rasterToMatch = NULL){
  
  ageMap <- raster::raster(file.path(dataPath,"can_age04_1km.tif")) %>%
    postProcess(targetFilePath = file.path(dataPath,"can_age04_1km.tif"), destinationPath = dataPath,
                studyArea = sim$studyArea, rasterToMatch = rasterToMatch)
  ageMap[] <- round(ageMap[], 0)
  cols <- length(which(!is.na(unique(getValues(ageMap)))))
  ageMap <- setColors(ageMap, n=cols,colorRampPalette(c("LightGreen", "DarkGreen"))(cols))
  
  return(ageMap)
}
