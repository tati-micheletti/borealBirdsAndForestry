fakeFocalRasterYears <- function(st = start(sim),
                            ed = end(sim),
                            res = c(250, 250),
                            crsRas = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"){
  
  focalYearList <- SpaDES.tools::gaussMap(raster::raster(xmn = -30^3, xmx = 30^3,
                                                             ymn = -30^3, ymx = 30^3,
                                                             resolution = res))
  raster::crs(focalYearList) <- crsRas
  Years <- c(0, st:ed)
  if (st > 1000){
    Years <- Years - 1900
    Years[1] <- 0
  }
  bff <- (max(focalYearList[]) - min(focalYearList[]))/length(Years)
  vecM <- numeric(length(Years))
  y <- Years[1]
  mult <- 0
  for (i in seq(from = 1, to = length(Years)*3, by = 3)){
    if (i == 1){
      vecM[i:(2+i)] <- c(bff*mult, bff*(mult+1), 0)
      mult <- mult+1
      y <- Years[2]
    } else {
      vecM[i:(2+i)] <- c(bff*mult, bff*(mult+1), y)
      mult <- mult+1
      y <- y + 1
    }
  }
  rclmat <- matrix(vecM, ncol=3, byrow=TRUE)
  rclmat[7,3] <- 0
  rclmat[1,3] <- 90
  focalYearList <- raster::reclassify(x = focalYearList,
                                          rcl = rclmat)
  focalYearList <- lapply(X = Years, FUN = function(yrs){
    yearValue <- raster::getValues(focalYearList)
    yearValue[yearValue %in% yrs] <- 999
    focalYearList <- raster::setValues(focalYearList, yearValue)
    ras <- raster::mask(x = focalYearList, mask = focalYearList,
                     maskvalue = 999, inverse = TRUE, updatevalue = 0)
    focalMatrix <- raster::focalWeight(x = focalYearList, 
                                       d = 4*res[1])
    focalYearList <- raster::focal(x = focalYearList, w = focalMatrix, 
                                   na.rm = TRUE)
    focalYearList[] <- focalYearList[]/max(focalYearList[], na.rm = TRUE)
    vals <- raster::getValues(focalYearList)
    vals[vals < 0.85] <- 0
    focalYearList <- raster::setValues(focalYearList, vals)
    focalYearList@data@names <- paste0("Year", yrs+1900)
    return(focalYearList)
  })
  focalYearList <- focalYearList[-1]
  return(focalYearList)
}