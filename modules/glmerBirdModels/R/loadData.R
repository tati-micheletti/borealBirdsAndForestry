# loadData Function

#CROPS (IF NECESSARY) AND LOAD DATA

loadData <- function(data){
  
  require(data.table)
  require(raster)
  require(sf)
  
  birdData <- fread(file.path(getwd(), "data", data))
  
  if (sim$cropForModel==TRUE){
    if(checkObject(sim = sim, name = "studyArea")==FALSE)
      sim$studyArea <- loadStudyArea(data = studyArea)
    
    studyArea <- sf::st_transform(x = sim$studyArea, crs = "+init=epsg:4326")
    
    xmin <- raster::extent(studyArea)[1]
    xmax <- raster::extent(studyArea)[2]
    ymin <- raster::extent(studyArea)[3]
    ymax <- raster::extent(studyArea)[4]
    
    birdData <- birdData[birdData$X>xmin & birdData$X<xmax &
                   birdData$Y>ymin & birdData$Y<ymax,]
    
    if (nrow(birdData)==0){
      stop("The selected area doesn't contain data. Try increasing the area.")
    }
  }
  
  return(birdData)
}