fetchData <- function(sim, birdSp) {
  
  require(raster)
  
  dataRaster <- vector(mode = "list", length = length(birdSp))
  names(dataRaster) <- as.list(birdSp)
  
  for (i in 1:length(birdSp)) {

    dataDir <- file.path(modulePath(sim),"birdDensityBCR_Prov_LCC/data")
    
    if(!paste0(birdSp[i],"_currmean.asc") %in% list.files(dataDir)){
      temp <- tempfile()
      download.file(paste0("https://s3-us-west-2.amazonaws.com/bam-databasin-climatechangepredictions/climatepredictions-ascii/",
                           birdSp[i],"_current.zip"),destfile = temp, cacheOK = TRUE)
      unzip(temp, exdir = dataDir)
    }
    
    dataRaster[[paste0(birdSp[i])]] <- raster::raster(file.path(dataDir,paste0(birdSp[i],"_currmean.asc")))

  }
  
  # For log values in the raster
  logDataRaster <- lapply(X = dataRaster, FUN = function(x){
    raster::values(x) <- log(raster::values(x))
    return(x)
  })

  return(logDataRaster)
  
}

