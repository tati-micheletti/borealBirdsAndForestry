tileReorder <- function(inList = newlist, origList = rasterList, c, pathData = pathData, ...){

  #Subset corresponding tiles 
  tilelist <- lapply(inList, '[[', c)
  names(tilelist) <- names(origList)

  #reclassify and mask them (and run focal distances)
  processed <- LCReclassify(inputTiles = tilelist, pathData = pathData, ...)
  
  return(processed)
}