extractValuesToTable <- function(destinationPath, shape, field, templateRas, extractFrom){
  library("sf")
  library("fasterize")
  library("data.table")
  sfShape <- st_as_sf(shape)
  rasShape <- fasterize(sf = sfShape, raster = templateRas, field = field)
  managedArea <- postProcess(rasShape, studyArea = extractFrom, rasterToMatch = templateRas,
                             filename2 = NULL, destinationPath = destinationPath)
  extracted <- data.table::data.table(pixelID = 1:ncell(managedArea), 
                                            value = raster::getValues(managedArea))
  extracted <- na.omit(extracted)
  return(extracted)
}