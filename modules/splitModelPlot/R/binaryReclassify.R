binaryReclassify <- function(inFile, inValues){
  
  tempVal <- raster::getValues(inFile)
  isClass <- tempVal %in% inValues
  NAClass <- is.na(tempVal)
  tempVal[isClass] <- 1
  tempVal[!isClass & !NAClass] <- 0
  outFile <- setValues(inFile, tempVal)
  return(outFile)
}
