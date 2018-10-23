fitModel <- function(inRas = stack, 
                     inputModel = models, 
                     spName = spName,
                     tileYear = currentTime){

  browser()

  # inRas[[2]][] <- inRas[[2]][]/1000 # Returning density to its original value: log(density_in_absence_disturbance) NO, values in original value
  # But need to log?
  # Its a list of birds, need to recover raster::raster()

  #Raster predict will work with any model that works with 'predict'
  if ("glmerMod" %in% class(inputModel)){
    prediction <- predict(object = inRas,
                          model = inputModel,
                          re.form = NA,
                          type = "response") # re.form = NA drops the random effects from the models; "response" gives us the density, not log
    names(prediction) <- paste0("prediction", spName, tileYear)
  } else {
    if ("glm" %in% class(inputModel)){
      tempRas <- raster::raster(nrows = nrow(inRas[[1]]), ncols = ncol(inRas[[1]]), 
                                crs = raster::crs(inRas[[1]]), ext = extent(inRas[[1]]), 
                                resolution = res(inRas[[1]]), vals = 0)
      names(tempRas) <- paste0("OFFSET_", spName)
      assign(x = paste0("OFFSET_", spName), value = tempRas)
      inRas <- addLayer(x = inRas, get(paste0("OFFSET_", spName)))
      prediction <- predict(object = inRas,
                            model = inputModel,
                            type = "response") # re.form = NA drops the random effects from the models; "response" gives us the density, not log
      names(prediction) <- paste0("prediction", spName, tileYear)
    }
  }
  prediction[] <- prediction[]*1000 # Results are already in density scale and multiplied by 1000 for saving space
  storage.mode(prediction[]) = "integer"
  
  message(crayon::green(paste0(spName, " prediction finalized for ", tileYear)))

  return(prediction)
}

