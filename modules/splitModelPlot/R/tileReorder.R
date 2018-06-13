tileReorder <- function(inList = newlist, origList = rasterList, c, ...){

  #Subset corresponding tiles 
  tilelist <- lapply(inList, '[[', c)
  names(tilelist) <- names(origList)
  
  #reclassify and mask them (and run focal distances)
  processed <- LCReclassify(inputTiles = tilelist, ...)
  
  return(processed)
}