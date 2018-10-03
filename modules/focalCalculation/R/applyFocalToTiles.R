applyFocalToTiles <- function(useParallel = P(sim)$useParallel, # Should do paralell only for focal and predicting, maybe?
                                  listTilePaths = sim$listTilePaths,
                                  pathData = dataPath(sim),
                                  pathCache = cachePath(sim), 
                                  startTime = start(sim), 
                                  endTime = end(sim),
                                  forestClass = P(sim)$forestClass,
                                  focalDistance = P(sim)$focalDistance,
                                  disturbanceClass = P(sim)$disturbanceClass,
                                  recoverTime = P(sim)$recoverTime,
                                  ressampledRes = P(sim)$ressampledRes){
  
  browser()
  # Subset matching tiles
  message(crayon::green("Tiles organized..."))
  totalTiles <- unique(lengths(listTilePaths))
  lengthVector <- 1:totalTiles
  orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
    sbset <- unlist(lapply(listTilePaths, `[[`, index), use.names = FALSE)
    return(sbset)
  })
  names(orderedRasterList) <- paste0("tile", lengthVector)
  
  # Entering each tile group
  reprojectedAssambledTiles <- lapply(X = lengthVector, FUN = function(tiles){
    message(crayon::red(paste0("Starting nested functions on ", 
                               names(orderedRasterList)[tiles], 
                               " of ", totalTiles)))
    assign(x = paste0(names(orderedRasterList)[tiles]),
           value = lapply(X = 1:length(listTilePaths), FUN = function(eachTile) {
             ras <- raster::raster(orderedRasterList[[tiles]][eachTile])
             rasterName <- ras@data@names
             ras[] <- ras[] %>% # Bring raster to memory, faster processing
               round(0)  # Round to 0, useful for integer rasters, makes them smaller
             storage.mode(ras[]) <- "integer" # Makes rasters smaller
             
             if (eachTile == 1) {
               # TILES 1 TYPE
               binaryDisturb <- binaryReclassify(inFile = ras, inValues = disturbanceClass)
               storage.mode(binaryDisturb[]) <- "integer" # Reducing size of raster by converting it to a real binary
               
             } else {
               if (eachTile == 2) {
                 # TILES 2 LC
                 binaryLC <- binaryReclassify(inFile = ras, inValues = forestClass)
                 storage.mode(binaryLC[]) <- "integer" # Reducing size of raster by converting it to a real binary
                 
               } else {
                 # TILES 3 YEAR
               }
             }
             
             return(ras)
           }
             )
           )
  })
    tileName <- get(names(orderedRasterList)[tiles])
    tileStack <- raster::stack(x = tileName) # Dive into Ian's functions. There is the matter of focal in areas that surround non-forest, etc... 
    #  Bring his functions here. As I have to work with these in memory and I obviously can't bring all of them to memory... 
    
    browser()
  
  
  # #Old function below
  # #=============================================================
  # 
  # 
  # browser() # TRY BRINGING ALL 3 INTO MEMORY. 
  # sim$Raster1[] <- round(sim$Raster1[], 0)
  # storage.mode(sim$Raster1[]) = "integer"
  # 
  # tileNumber <- x 
  # 
  # #Subset corresponding tiles 
  # tilelist <- lapply(inList, '[[', x)
  # names(tilelist) <- origList
  # 
  # if (!all(is.na(tilelist$birdDensityRasters[])) == TRUE){
  #   
  #   #reclassify and mask them (and run focal distances)
  #   # processed <- LCReclassify(inputTiles = tilelist, 
  #   #                           pathData = pathData, 
  #   #                           pathCache = pathCache, 
  #   #                           startTime = startTime,
  #   #                           endTime = endTime,
  #   #                           forestClass = forestClass,
  #   #                           focalDistance = focalDistance,
  #   #                           disturbanceClass = disturbanceClass,
  #   #                           spName = spName,
  #   #                           passedModel = passedModel,
  #   #                           tileNumber = tileNumber,
  #   #                           recoverTime = recoverTime,
  #   #                           maxTile = maxTile)
  #   
  #   return(processed)
  #   
  # } else {
  #   
  #   message(crayon::red(paste0("Tile ",  tileNumber, " of ",
  #                              maxTile, " tiles for ", spName,
  #                              " was skipped as it has only NA values (Time: ", Sys.time(), ")")))
  #   
  # }
}