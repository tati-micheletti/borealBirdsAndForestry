fitModel <- function(inRas, 
                     inputModel, 
                     x,
                     tileYear){

  #Raster predict will work with any model that works with 'predict'
  if ("glmerMod" %in% class(inputModel)){
    prediction <- predict(object = inRas,
                          model = inputModel,
                          re.form = NA,
                          type = "response") # re.form = NA drops the random effects from the models; "response" gives us the density, not log
    names(prediction) <- paste0("prediction", x, tileYear)
  } else {
    if ("glm" %in% class(inputModel)){
      tempRas <- raster::raster(nrows = nrow(inRas[[1]]), ncols = ncol(inRas[[1]]), 
                                crs = raster::crs(inRas[[1]]), ext = extent(inRas[[1]]), 
                                resolution = res(inRas[[1]]), vals = 0)
      off <- names(inputModel$data)[grepl(pattern = "OF", x = names(inputModel$data)) & 
                                    grepl(pattern = x, x = names(inputModel$data))]
      names(tempRas) <- off
      assign(x = off, value = tempRas)
      inRas <- addLayer(x = inRas, get(off))
      prediction <- predict(object = inRas,
                            model = inputModel,
                            type = "response") # No re.form in glm
      names(prediction) <- paste0("prediction", x, tileYear)
    }
  }
  # I will try without first... If it doesn't fir, we multiply
  #prediction[] <- prediction[]*1000 # Results are already in density scale and multiplied by 1000 for saving space
  #suppressWarnings(storage.mode(prediction[]) <- "integer")
  
  message(crayon::green(paste0(x, " prediction finalized for year ", tileYear)))
  
  return(prediction)
}

