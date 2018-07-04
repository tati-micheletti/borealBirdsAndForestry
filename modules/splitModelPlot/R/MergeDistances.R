MergeDistances <- function(inList = fDistanceLists, 
                           times = times, 
                           birdDensityRasters = inputTiles$birdDensityRasters,
                           passedModel = models,
                           intermPath = intermPath){
  
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
  
  # Define a directory for the intermediate rasters
  # dir.create(file.path(intermPath, "intermediateRasters"), showWarnings = FALSE)
  # intermPathFull <- file.path(intermPath, "intermediateRasters")

  # Save first (years) raster tiles to disk with a randomly generated name
  first <- names(out)[1]
  firstYear <- out[[first]]
  # dir.create(file.path(intermPathFull, "firstYear"), showWarnings = FALSE)
  # tmpFile <- tempfile(pattern = "firstYear", tmpdir = file.path(intermPathFull, "firstYear"), fileext = ".tif")
  # writeRaster(x = firstYear, filename = tmpFile)
  
  # Save last (years) raster tiles to disk with a randomly generated name
  last <- names(out)[length(out)]
  lastYear <- out[[last]]
  # dir.create(file.path(intermPathFull, "lastYear"), showWarnings = FALSE)
  # tmpFile <- tempfile(pattern = "lastYear", tmpdir = file.path(intermPathFull, "lastYear"), fileext = ".tif")
  # writeRaster(x = lastYear, filename = tmpFile)
  
  #Stack rasters in the list
  predictedStack <- raster::stack(out)
  arrayStack <- raster::as.array(predictedStack)

  # Slope coefficient's raster
  slopeCoefficientVal <- apply(X = arrayStack, MARGIN = c(1,2), FUN = function(x){
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
    raster::setValues(slopeCoefficientVal)
  
  # dir.create(file.path(intermPathFull, "slopeCoefficient"), showWarnings = FALSE)
  # tmpFile <- tempfile(pattern = "slopeCoefficient", tmpdir = file.path(intermPathFull, "slopeCoefficient"), fileext = ".tif")
#  writeRaster(x = slopeCoefficient, filename = tmpFile) # Writing to disk shouldn't happen here yet!
  # slopeCoefficient <- tmpFile
  
  # Signifficancy of coefficient's raster
  slopeSignificancyVal <- apply(X = arrayStack, MARGIN = c(1,2), FUN = function(x){
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
  
  slopeSignificancy <- predictedStack[[1]] %>%
    raster::setValues(slopeSignificancyVal)
  # dir.create(file.path(intermPathFull, "slopeSignificancy"), showWarnings = FALSE)
  # tmpFile <- tempfile(pattern = "slopeSignificancy", tmpdir = file.path(intermPathFull, "slopeSignificancy"), fileext = ".tif")
  # writeRaster(x = slopeSignificancy, filename = tmpFile)
  # slopeSignificancy <- tmpFile

  modPredict <- list(firstYear, lastYear, slopeSignificancy, slopeCoefficient)
  modPredict2 <- lapply(X = modPredict, FUN = function(x){
    expValues <- round(x[]*1000, 0) # Here is where we multiply to convert all to integer. 
    x <- raster::setValues(x, expValues) # And convert all to intger "INT2S" (-32.000 to 32.000)
    dataType(x) <- "INT2S"
    return(x)
  })
  
  names(modPredict2) <- c("firstYear", "lastYear", "slopeSignificancy", "slopeCoefficient")
  
  return(suppressWarnings(modPredict2))
}
