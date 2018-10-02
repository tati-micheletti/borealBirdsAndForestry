tileReorder <- function(lengthVector = 1:(sim$nx * sim$ny),
                        useParallel = P(sim)$useParallel,
                        rastersList = sim$rastersList,
                        pathData = dataPath(sim),
                        intermPath = cachePath(sim), 
                        startTime = start(sim), 
                        endTime = end(sim),
                        forestClass = P(sim)$forestClass,
                        # focalDistance = focalDistance,
                        disturbanceClass = P(sim)$disturbanceClass,
                        # spName = spName,
                        # passedModel = models,
                        recoverTime = recoverTime,
                        maxTile = sim$nx * sim$ny){
  
  #Subset corresponding tiles
  orderedRasterList <- lapply(rastersList, '[[', x)
  names(orderedRasterList) <- names(rastersList)
  
  
  
  #=============================================================

  
  browser() # TRY BRINGING ALL 3 INTO MEMORY. 
  sim$Raster1[] <- round(sim$Raster1[], 0)
  storage.mode(sim$Raster1[]) = "integer"
  
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