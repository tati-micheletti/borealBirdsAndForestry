returnDTfromShapefile <- function(shp, 
                                  rasterToMatch,# Template for rasterize
                                  fieldToUse = NULL, # Which field from the shapefile should it use to extract the data for the table?
                                  columnName, # Name of the column to identify the sites/shapefiles (i.e.forests will have 'Managed' and 'Unmanaged')
                                  namesVector = NULL, # Names that you want in the final column 
                                  plotRas = FALSE){
  if (is.null(fieldToUse)){
     shp$newField <- 1
     fieldToUse <- "newField"
  }
  if (is.null(namesVector)){
    if (fieldToUse == "newField") {
      namesVector <- columnName
    } else {
      namesVector <- shp[[fieldToUse]]
    }
  }
  # namesVector: need to be the length matching the vector that you want
  # Need to rasterize the shapefile for getting pixelID
  SF <- sf::st_as_sf(x = shp)
  
  # Need to create a table with the whole vector 
  tb <- data.table(vec = SF[[fieldToUse]],
                   field = as.numeric(as.factor(SF[[fieldToUse]])))
  SF$field <- tb$field

  RAS <- fasterize::fasterize(sf = SF, 
                              raster = rasterToMatch,
                              field = "field")
  if (plotRas)
    Plot(RAS, title = columnName)
  message(paste0("building table for", columnName))
  finalTable <- data.table::data.table(1:ncell(RAS),
                                       as.character(raster::getValues(RAS)))
  names(finalTable) <- c("pixelID", columnName)
  finalTable <- na.omit(finalTable)
  
  if (!is.null(namesVector)){
    if (all(length(namesVector) != length(SF[[fieldToUse]]), 
             length(namesVector) > 1))
      stop("You need to provide a 'namesVector' that matches the field you passed in length")
    convTable <- data.table(as.character(SF$field), namesVector)
  } else {
    print("Entering debug mode: namesVector can't be NULL")
    browser()
  }
      invisible(apply(X = convTable, MARGIN = 1, FUN = function(Row){
      finalTable[get(columnName) == Row[1], shape := Row[2]]
    }))
      finalTable[, (columnName) := NULL]
      # change the name
      names(finalTable)[names(finalTable)=="shape"] <- columnName
  return(finalTable)
}

