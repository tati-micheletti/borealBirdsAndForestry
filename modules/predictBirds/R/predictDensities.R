predictDensities <- function(birdSpecies = sim$birdSpecies,
                             disturbanceRas = sim$focalYearList[[paste0("Year", time(sim))]],
                             birdDensityRasters = sim$birdDensityRasters,
                             currentTime = time(sim),
                             models = sim$models,
                             pathData = dataPath(sim)) {

predictionPerSpecies <-  lapply(birdSpecies, function(spName){
  message(crayon::yellow("Stacking rasters for ", spName , " prediction"))
  birdD <- raster::raster(birdDensityRasters[[spName]])
  valsD <- log(raster::getValues(birdD)) # log the value of densities so it is the same of the original model
  valsD[valsD < -0.9] <- -1
  birdD <- raster::setValues(birdD, valsD)
  rm(valsD)
  gc()
  stackRas <- raster::stack(disturbanceRas, birdD) # Might need to remove individual rasters here
  if ("glmerMod" %in% class(models)){
    names(stackRas)[1] <- names(models@frame)[2]
    names(stackRas)[2] <- names(models@frame)[3]
  } else {
    if ("glm" %in% class(models)){
      names(stackRas)[1] <- names(models$coefficients)[2]
      names(stackRas)[2] <- names(models$coefficients)[3]
    }
  }
  predicted <- fitModel(inRas = stackRas, inputModel = models, spName = spName, tileYear = currentTime)
  suppressWarnings(dir.create(file.path(pathData, "predicted")))
  predictedName <- file.path(pathData, paste0("predicted/predicted", spName, currentTime, ".tif"))
  raster::writeRaster(x = predicted, filename = predictedName, format = "GTiff", overwrite = TRUE)
  predicted <- raster(predictedName)
  gc()
  return(predicted) # The predicted value is multiplied by 1000 and stored as integer
})
names(predictionPerSpecies) <- birdSpecies
  return(predictionPerSpecies)
}
