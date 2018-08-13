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
                        recoverTime = recoverTime,
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
                              recoverTime = recoverTime,
                              maxTile = maxTile)

    return(processed)
    
  } else {
    
    message(crayon::red(paste0("Tile ",  tileNumber, " of ",
                               maxTile, " tiles for ", spName,
                               " was skipped as it has only NA values (Time: ", Sys.time(), ")")))

  }
}