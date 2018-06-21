fetchData <- function(pathData = dataPath(sim), 
                      birdSp = sim$birdSpecies, studyArea = sim$rP) {
  require(raster)

    dataRaster <- lapply(X = birdSp, FUN = function(x){
      ras <- prepInputs(targetFile = paste0(x, "_currmean.asc"),
                        archive = paste0(x, "_current.zip"),
                        url = paste0("https://s3-us-west-2.amazonaws.com/bam-databasin-climatechangepredictions/climatepredictions-ascii/",
                                     x, "_current.zip"),
                        destinationPath = pathData,
                        studyArea = studyArea)
    })
 
  # For log values in the raster
  logDataRaster <- lapply(X = dataRaster, FUN = function(x){
    raster::values(x) <- log(raster::values(x))
    return(x)
  })

  return(logDataRaster)
  
}

