fitModel <- function(inRas, inputModel){
  browser()
  #To use the raster function, coefficients must match names of raster layer. Below only works if order of distance parameters matches model coefficients
  
  # names(inRas) <- names(inputModels$coefficients[2:(1+length(names(inRas)))])
  names(inRas)[2] <- names(inputModel@frame)[3]
  names(inRas)[1] <- paste("State_P_", 100, sep = "") #Make this P(sim)$focalDistance
  #Raster predict will work with any model that works with 'predict'
  prediction <- predict(object = inRas, model = inputModel, na.rm = TRUE)
  
  return(prediction)
}

