fitModel <- function(inRas = tempStack, 
                     inputModel = passedModel, 
                     spName = spName,
                     tileYear = tileYear){
  
  # To use the raster function, coefficients must match names of raster layer. 
  # Below only works if order of distance parameters matches model coefficients
  names(inRas)[1] <- names(inputModel@frame)[2] #Make this P(sim)$focalDistance
  names(inRas)[2] <- names(inputModel@frame)[3]

  inRas[[2]][] <- inRas[[2]][]/1000 # Returning abundance to its original value: log(abundance_in_absence_disturbance)

  #Raster predict will work with any model that works with 'predict'
  tryCatch({
    prediction <- predict(object = inRas,
                          model = inputModel,
                          re.form = NA) # re.form = NA drops the random effects from the models
  },
    error = function(e) {
      NA
    }
  )
  
  prediction[] <-exp(prediction[])*1000 # Results are already in abundance scale and multiplied by 1000 for saving space
  storage.mode(prediction[]) = "integer"
  
  message(crayon::green(paste0(spName, " prediction finalized for ", tileYear)))

  return(prediction)
}

