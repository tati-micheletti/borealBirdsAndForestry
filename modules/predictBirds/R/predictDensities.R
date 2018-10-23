predictDensities <- function(birdSpecies = sim$birdSpecies,
                             disturbanceRas = sim$focalYearList[[paste0("Year", time(sim))]],
                             birdDensityRasters = sim$birdDensityRasters,
                             currentTime = time(sim),
                             models = sim$models) {
  
predictionPerSpecies <-  lapply(birdSpecies, function(spName){
  message(crayon::yellow("Stacking rasters for ", spName , " prediction"))
  browser()
  birdD <- birdDensityRasters[[spName]]
  stackRas <- raster::stack(disturbanceRas, birdD)
  if ("glmerMod" %in% class(models)){
    names(stackRas)[1] <- names(models@frame)[2] #Make this P(sim)$focalDistance
    names(stackRas)[2] <- names(models@frame)[3]
  } else {
    if ("glm" %in% class(models)){
      names(stackRas)[1] <- names(models$coefficients)[2] #Make this P(sim)$focalDistance
      names(stackRas)[2] <- names(models$coefficients)[3]
    }
  }
  predicted <- fitModel(inRas = stackRas, inputModel = models, spName = spName, tileYear = currentTime)
  browser() # CHeck names... should return a list of years, and in each year, list of species?! Think...
  return(predicted)
})
  return()
}
