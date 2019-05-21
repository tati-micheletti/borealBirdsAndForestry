predictDensities <- function(birdSpecies,
                             disturbanceRas,
                             birdDensityRasters,
                             currentTime,
                             modelList,
                             pathData) {

predictionPerSpecies <-  lapply(birdSpecies, function(spName){ # I can paralellize the predictions up to 3 times safely apparently. It in only consuming 16Gb per species
  message(crayon::yellow("Stacking rasters for ", spName , " prediction"))
  suppressWarnings(dir.create(file.path(pathData, "predicted")))
  models <- modelList[[spName]]
  if ("glmerMod" %in% class(models)){
    nameStackRas1 <- names(models@frame)[2]
    nameStackRas2 <- names(models@frame)[3]
  } else {
    if ("glm" %in% class(models)){
      nameStackRas1 <- names(models$coefficients)[2]
      nameStackRas2 <- names(models$coefficients)[3]
    }
  }
  focDis <- as.numeric(gsub("[^\\d]+", "", nameStackRas1, perl=TRUE))
  predictedName <- file.path(pathData, paste0("predicted/predictedFocal", focDis, "m", spName, currentTime, ".tif"))
  if (!file.exists(predictedName)){
  birdD <- raster::raster(birdDensityRasters[[spName]])
  valsD <- log(raster::getValues(birdD)) # log the value of densities so it is the same of the original model
  valsD[valsD < -0.99] <- -1
  birdD <- raster::setValues(birdD, valsD)
  rm(valsD)
  gc()
  if (any(!identical(round(raster::extent(x = disturbanceRas), 10^-100), round(raster::extent(birdD), 10^-100)),
          !identical(raster::res(x = disturbanceRas), raster::res(birdD)),
          !identical(raster::crs(x = disturbanceRas), raster::crs(birdD)))){
    fixedDensityRas <- file.path(pathData, paste0("density", spName, "res", res(disturbanceRas)[1], "m.tif"))
    if (file.exists(fixedDensityRas)){
      birdD <- raster::raster(fixedDensityRas)
    } else {
      birdD <- postProcess(x = birdD, rasterToMatch = disturbanceRas,
                           filename2 = tools::file_path_sans_ext(fixedDensityRas),
                           format = "GTiff", overwrite = TRUE, useCache = FALSE)
      raster::extent(birdD) <- raster::alignExtent(extent = raster::extent(birdD), 
                                                   object = disturbanceRas, snap = "near")
      # Don't think I will need the one below anymore.
      # disturbanceRas <- postProcess(x = disturbanceRas, rasterToMatch = birdD,
      #                              maskWithRTM = TRUE, 
      #                              filename2 = file.path(pathData, "predicted", paste0(disturbanceRas@data@names, "Fixed")),
      #                              format = "GTiff", overwrite = TRUE, useCache = FALSE)
    }
  }
  stackRas <- raster::stack(disturbanceRas, birdD) # Might need to remove individual rasters here
    names(stackRas)[1] <- nameStackRas1
    names(stackRas)[2] <- nameStackRas2
  suppressWarnings(predicted <- fitModel(inRas = stackRas, 
                                         inputModel = models, 
                                         spName = spName, 
                                         tileYear = currentTime))
  raster::writeRaster(x = predicted, filename = predictedName, 
                      format = "GTiff", overwrite = TRUE)
  }
  predicted <- raster(predictedName)
  gc()
  return(predicted) # The predicted value is NOT multiplied by 1000! For that, need to change fitModel!
})
names(predictionPerSpecies) <- birdSpecies
return(predictionPerSpecies)
}
