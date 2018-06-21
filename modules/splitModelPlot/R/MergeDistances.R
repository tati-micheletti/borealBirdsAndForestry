MergeDistances <- function(inList = fDistanceLists, 
                           times = times, 
                           birdDensityRasters = inputTiles$birdDensityRasters,
                           passedModel = models,
                           pathData = pathData){
  
  mergeList <- rlist::list.cbind(inList) %>%
    list.parse(.)
  
  #Make raster stack with focal distance rasters for every year. 
  stackDistances <- lapply(mergeList, FUN = stack)
  
  #for each raster in stackDistances (ie focal/year combo) fit model 
    #Make a stack of the disturbed area in a single year and the expected abundance. Use to predict with fitModel   
  out <- lapply(stackDistances, function(q, abundRas = birdDensityRasters){
    names(birdDensityRasters) <- 'abundance'
    tempStack <- stack(q, abundRas) 
    p <- fitModel(inRas = tempStack, inputModel = passedModel)
    return(p)
  })
  
  # Calculate population variation, saving the tile to disk with a randomly generated name
  first <- names(out)[1]
  last <- names(out)[length(out)]
  propChange <- (out[[first]] - out[[last]]) / out[[first]]
  dir.create(file.path(pathData, "propChange"), showWarnings = FALSE)
  tmpFile <- tempfile(pattern = "propChange", tmpdir = file.path(pathData, "propChange"), fileext = ".tif")
  writeRaster(x = propChange, filename = tmpFile)
  propChange <- tmpFile
  
  #Stack rasters in the list
  predictedStack <- raster::stack(out)
  arrayStack <- raster::as.array(predictedStack)

  # Slope coefficient's raster
  slopeCoefficient <- apply(X = arrayStack, MARGIN = c(1,2), FUN = function(x){
    dfX <- data.frame(x, times)
    mod <- tryCatch({
      slpCoef <- lm(x ~ times, data = dfX,  na.action = na.omit)
      coef <- coef(summary(slpCoef))["times","Estimate"]
      },
      error = function(e) {
       NA
      })
    return(mod)
  })
  
  slopeCoefficient <- predictedStack[[1]] %>%
    raster::setValues(slopeCoefficient)
  
  dir.create(file.path(pathData, "slopeCoefficient"), showWarnings = FALSE)
  tmpFile <- tempfile(pattern = "slopeCoefficient", tmpdir = file.path(pathData, "slopeCoefficient"), fileext = ".tif")
  writeRaster(x = slopeCoefficient, filename = tmpFile)
  slopeCoefficient <- tmpFile
  
  # Signifficancy of coefficient's raster
  slopeP <- apply(X = arrayStack, MARGIN = c(1,2), FUN = function(x){
    dfX <- data.frame(x, times)
    mod <- tryCatch({
      slpCoef <- lm(x ~ times, data = dfX,  na.action = na.omit)
      coef <- coef(summary(slpCoef))["times","Pr(>|t|)"]
    },
    error = function(e) {
      NA
    })
    return(mod)
  })
  
  slopeP <- predictedStack[[1]] %>%
    raster::setValues(slopeP)
  dir.create(file.path(pathData, "slopeP"), showWarnings = FALSE)
  tmpFile <- tempfile(pattern = "slopeP", tmpdir = file.path(pathData, "slopeP"), fileext = ".tif")
  writeRaster(x = slopeP, filename = tmpFile)
  slopeP <- tmpFile

  # REVISE THIS!!! Should I return the list? Only if this is a pointer list!!! Otherwise, raster names
  modPredict <- list(propChange, slopeP, slopeCoefficient)
  names(modPredict) <- c("populationChange", "slopeSignificancy", "slopeCoefficient")
  
  return(suppressWarnings(modPredict))
}
