fitModel <- function(inRas, 
                     inputModel, 
                     x,
                     tileYear){

  # Not a raster predict anymore. This is now a data.frame predict.
  if ("glmerMod" %in% class(inputModel)){
    prediction <- predict(object = inputModel,
                          newdata = inRas,
                          re.form = NA,
                          type = "response") # re.form = NA drops the random effects from the models; "response" gives us the density, not log
    attr(prediction, "prediction") <- paste0(x, tileYear)
  } else {
    if ("glm" %in% class(inputModel)){
      tempCol <- rep(0, times = nrow(inRas))
      off <- names(inputModel$data)[grepl(pattern = "OF", x = names(inputModel$data)) & 
                                    grepl(pattern = x, x = names(inputModel$data))]
      origNames <- names(inRas)
      inRas <- cbind(inRas, tempCol)
      names(inRas) <- c(origNames, off)

      prediction <- predict(newdata = inRas,
                            object = inputModel,
                            type = "response") # No re.form in glm
      attr(prediction, "prediction") <- paste0(x, tileYear)
    }
  }
  # I will try without first... If it doesn't fit, we multiply
  #prediction[] <- prediction[]*1000 # Results are already in density scale and multiplied by 1000 for saving space
  #suppressWarnings(storage.mode(prediction[]) <- "integer")
  
  message(crayon::green(paste0(x, " prediction finalized for year ", tileYear)))
  
  return(prediction)
}

