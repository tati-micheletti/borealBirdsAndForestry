# Extracting the values from the rasters to feed the bird predictions

extractData <- function(dataset.raster){
  data <- data.frame(coordinates(dataset.raster),
                     raster::values(dataset.raster))
  names(data) <- c("X", "Y", "Value")  

  ras <- data

return(ras)

}