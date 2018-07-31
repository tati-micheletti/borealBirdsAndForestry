tileReorder <- function(x,
                        inList = newlist, 
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
                        maxTile = length(lengthvect)){

    tileNumber <- x 

  #Subset corresponding tiles 
  tilelist <- lapply(inList, '[[', x)
  names(tilelist) <- origList

  if (!all(is.na(tilelist$birdDensityRasters[])) == TRUE){
    
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
                              passedModel = passedModel,
                              tileNumber = tileNumber,
                              maxTile = maxTile)

    return(processed)
    
  } else {
    
    browser()
    
    # If we merge them and all is good we don't need the code below. Otherwise, we can uncomment it and it should be good
  # processed <- lapply(X = tilelist, FUN = function(x){
  #   x[] <- NA
  #  return(x)
  # })
  # names(tilelist) <- c("firstYear","lastYear","slopeSignificancy","slopeCoefficient")
  # return(processed)

  }
}