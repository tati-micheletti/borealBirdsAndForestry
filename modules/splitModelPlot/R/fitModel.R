fitModel <- function(inRas, inputModel){
  browser()
  #To use the raster function, coefficients must match names of raster layer. Below only works if order of distance parameters matches model coefficients
  
  # names(inRas) <- names(inputModels$coefficients[2:(1+length(names(inRas)))])
  names(inRas) <- coefficients(inputModel)[1:2]
  
  #Raster predict will work with any model that works with 'predict'
  prediction <- predict(object = inRas, model = inputModel, na.rm = TRUE)
  
  return(prediction)
}

