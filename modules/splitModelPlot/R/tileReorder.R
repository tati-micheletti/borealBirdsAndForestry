tileReorder <- function(inList = newlist, 
                        origList = rasterList, 
                        pathData = pathData, 
                        intermPath = intermPath, 
                        startTime = startTime, 
                        endTime = endTime,
                        forestClass = forestClass,
                        focalDistance = focalDistance,
                        disturbanceClass = disturbanceClass,
                        spName = spName,
                        passedModel = models,
                        c = c){
  
  #Subset corresponding tiles 
  tilelist <- lapply(inList, '[[', c)
  names(tilelist) <- origList

  #reclassify and mask them (and run focal distances)
  processed <- LCReclassify(inputTiles = tilelist, 
                            pathData = pathData, 
                            intermPath = intermPath, 
                            startTime = startTime,
                            endTime = endTime,
                            forestClass = forestClass,
                            focalDistance = focalDistance,
                            disturbanceClass = disturbanceClass,
                            spName = spName,
                            passedModel = passedModel)
  
  return(processed)
}