corePrediction <- function(bird, model, birdDensityRas,
                           disturbanceRas = NULL,
                           successionLayers = NULL,
                           staticLayers = NULL,
                           currentTime,
                           pathData,
                           overwritePredictions = FALSE){
  
  successionLayersNames <- names(successionLayers)
  staticLayersNames <- names(staticLayers)
  message(crayon::yellow(paste0("Predicting for ", bird , ". Prediction for time ", currentTime)))
  
  if ("glmerMod" %in% class(model)){
    nameStackRas1 <- names(model@frame)[2]
    nameStackRas2 <- names(model@frame)[3]
  } else {
    if ("glm" %in% class(model)){
      nameStackRas1 <- names(model$coefficients)[2]
      nameStackRas2 <- names(model$coefficients)[3]
    } else {
      if ("gbm" %in% class(model)){ # If gbm, do everything in here, else, do outside
        browser() # NEED TO DEBUG HERE FOR BIRDS PARALLEL IN NWT PROJECT...
        tryCatch({
          stkLays <- raster::stack(successionLayers, staticLayers)
        }, error = function(e){
          stop("crs and or extents don't align. Check you layers have the same crs and projection before this call")
        })
        predictedName <- file.path(pathData, paste0("predicted/predicted", bird, "Year", currentTime, ".tif"))
        if (isTRUE(overwritePredictions)||!file.exists(predictedName)){
          message(crayon::yellow(paste0(" Starting prediction raster for ", bird, ". This might take some time... [", Sys.time(),"]")))
          startTime <- Sys.time()
          predicted <- gbm::predict.gbm(object = model, newdata = raster::as.data.frame(stkLays, row.names = TRUE),
                                        type = "response",
                                        n.trees = model$n.trees)
          message(crayon::green(paste0("Prediction finalized for ", bird, ". [", Sys.time(),"]. Total time elapsed: ", 
                                       Sys.time() - startTime)))
          startTime <- Sys.time()
          message(crayon::green("Masking ", bird , " prediction to ", crayon::red("studyArea"), " for time ", currentTime))
          basePlot <- stkLays[[1]]
          basePlot <- setValues(basePlot, predicted)
          basePlot <- reproducible::fastMask(basePlot, y = studyArea)
          
          names(basePlot) <- paste0("predicted", bird)
          message(crayon::green("Masking ", bird , " prediction to ", crayon::red("uplands"), " for time ", currentTime))
          predictedMasked <- reproducible::postProcess(x = basePlot, rasterToMatch = uplandsRaster, 
                                                       maskWithRTM = TRUE, destinationPath = pathData, filename2 = NULL)
          
        }
        predicted <- raster(predictedName)
        gc()
        return(predicted)
      }
    }
  }
  focDis <- as.numeric(gsub("[^\\d]+", "", nameStackRas1, perl=TRUE))
  predictedName <- file.path(pathData, paste0("predicted/predictedFocal", focDis, "m", bird, currentTime, ".tif"))
  if (isTRUE(overwritePredictions)||!file.exists(predictedName)){
    valsD <- log(raster::getValues(birdDensityRas)) # log the value of densities so it is the same of the original model
    valsD[valsD < -0.99] <- -1
    birdDensityRas <- raster::setValues(birdDensityRas, valsD)
    rm(valsD)
    gc()
    tryCatch({
      stkLays <- raster::stack(disturbanceRas, birdDensityRas) # Might need to remove individual rasters here
    }, error = function(e){
      stop("Please make sure the disturbanceRaster has the same crs, projection and alignment of the birdDensityRaster")
    })
    names(stkLays)[1] <- nameStackRas1
    names(stkLays)[2] <- nameStackRas2
    suppressWarnings(predicted <- fitModel(inRas = stkLays, 
                                           inputModel = model, 
                                           x = bird, 
                                           tileYear = currentTime))
  }
  predicted <- raster(predicted)
  names(predicted) <- paste0("predictedFocal", focDis, "m", bird, currentTime)
  gc()
  browser()
  return(predicted) # The predicted value is NOT multiplied by 1000! For that, need to change fitModel!
}