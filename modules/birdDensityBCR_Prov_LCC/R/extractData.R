# Extracting the values from the rasters to feed the bird predictions

extractData <- function(datasetRaster, typeData, list){
  
  require(data.table)
  require(raster)
  
  if (typeData == "list") {
    
    data <- rep(list(NA),length(list))
    
    for (i in 1:length(list)){

      names(data)[i] <- list[i]
      
        data[[paste0(list[i])]] <- data.table(sp::coordinates(datasetRaster[[paste0(list[i])]]),
                                              raster::values(datasetRaster[[i]]))
      names(data[[paste0(list[i])]]) <- c("X", "Y", "Value")
      
    }

  return(data)
  
  } else {
    
      data <- data.table(coordinates(datasetRaster),
                         raster::values(datasetRaster))
      names(data) <- c("X", "Y", "Value")  
      
      return(data)
  }

}