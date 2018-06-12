tileReorder <- function(inList, origList, c, ...){
  #Subset corresponding tiles 
  
  tilelist <- lapply(inList, '[[', c)
  names(tilelist) <- names(origList)
  #reclassify and mask them (and run focal distances)
  
  processed <- LCReclassify(tilelist, ...)
  
  return(processed)
}