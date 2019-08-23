calculateRange <- function(birdList = NULL, dataPath, rasterToMatch){
  source('/mnt/data/Micheletti/borealBirdsAndForestry/modules/habitatDensityMasking/R/createBirdsRangeRasters.R')
  source('/mnt/data/Micheletti/borealBirdsAndForestry/modules/habitatDensityMasking/R/getSpRange.R')
  library("reproducible")
  library("data.table")
  birdsRangeList <- createBirdsRangeRasters(birdList)
  rangeTable <- data.table::rbindlist(lapply(X = names(birdsRangeList), function(sp){
  spRange <- Cache(getSpRange, species = sp, 
                        birdsRangeList = birdsRangeList[[sp]], 
                        pathHR = dataPath, rasterToMatch = rasterToMatch)
    library("raster")
    dt <- data.table::data.table(species = sp, numberPixels = sum(spRange[spRange==1]), 
                                 areaHa = prod(res(spRange))/10000)
  }))
  return(rangeTable)
}
