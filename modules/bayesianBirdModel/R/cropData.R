cropData <- function(data = birdDataFinal,
                     rP = rP, originalDataInLatLong = TRUE) {
  
  if(originalDataInLatLong==TRUE){
    rP <- spTransform(x = rP, CRSobj = CRS("+init=epsg:4326"))
  }
  
  polyg <- rP
  polMinX <- extent(polyg)[1]
  polMaxX <- extent(polyg)[2]
  polMinY <- extent(polyg)[3]
  polMaxY <- extent(polyg)[4]
  
  finalDataset <- data[data$X>polMinX & data$X<polMaxX &
                         data$Y>polMinY & data$Y<polMaxY,]
  
  if (nrow(finalDataset)==0){
    stop("The selected area doesn't contain data. Try moving or increasing the area.")
  }
  
  return(invisible(finalDataset))
  
}