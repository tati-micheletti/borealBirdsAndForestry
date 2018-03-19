# loadData Function

#CROPS (IF NECESSARY) AND LOAD DATA

loadCroppedData <- function(sim, dataName){
  
  require(data.table)
  require(raster)
  require(sf)
  browser()
  if (params(sim)$glmerBirdModels$cropForModel==TRUE){
    
    # data.path <- file.path(getwd(), "modules/glmerBirdModels/data", data)
    
    if (is.null(sim$birdData)){ # Still to test for automatic download of data. If it doesn't work: try exists(data.path) and uncommenting line above
      invisible(readline(prompt="Make dure you have the dataset in Google Drives folder 'BAM', and press [enter] to continue"))
      require(googledrive)
      drive_download(file.path("BAM",data), path = file.path(getwd(), "modules/glmerBirdModels/data", data), overwrite = FALSE,verbose = FALSE)
      birdData <- fread(file.path(getwd(), "modules/glmerBirdModels/data", data))
    }
    
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