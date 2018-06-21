fitModel <- function(inRas = tempStack, 
                     inputModel = models){
  
  # To use the raster function, coefficients must match names of raster layer. 
  # Below only works if order of distance parameters matches model coefficients
  names(inRas)[1] <- names(inputModel[[1]]@frame)[2] #Make this P(sim)$focalDistance
  names(inRas)[2] <- names(inputModel[[1]]@frame)[3]
  
  #Raster predict will work with any model that works with 'predict'
  prediction <- predict(object = inRas, model = inputModel[[1]], re.form = NA)
  
  return(prediction)
}

