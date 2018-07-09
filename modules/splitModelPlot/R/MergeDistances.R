MergeDistances <- function(inList = fDistanceLists, 
                           times = times, 
                           birdDensityRasters = inputTiles$birdDensityRasters,
                           passedModel = passedModel,
                           intermPath = intermPath,
                           spName = spName){
  
  mergeList <- rlist::list.cbind(inList) %>%
    list.parse(.)
  
  #Make raster stack with focal distance rasters for every year.
  message(crayon::yellow("Stacking rasters for ", spName , " prediction"))
  stackDistances <- lapply(mergeList, FUN = stack)
  
  #for each raster in stackDistances (ie focal/year combo) fit model 
    #Make a stack of the disturbed area in a single year and the expected abundance. Use to predict with fitModel   

  out <- lapply(names(stackDistances), function(x){ # Single tile with all years
    tileYear <- x
    q <- stackDistances[[x]]
    names(birdDensityRasters) <- 'abundance'
    tempStack <- stack(q, birdDensityRasters)
    p <- fitModel(inRas = tempStack, inputModel = passedModel, spName = spName, tileYear = tileYear)
    return(p)
  })
  
  # Remove old tiles
  names(out) <- names(stackDistances)
  rm(birdDensityRasters, stackDistances, mergeList)
  
  message(crayon::yellow(paste0("Extracting results from first and last years of predictions for ", spName)))
  
  # Save first (years) raster tiles to disk with a randomly generated name
  first <- names(out)[1]
  firstYear <- out[[first]] # left the code like this because it is easier to name the rasters in case I need to save them to disk
  firstYear[] <- round(firstYear[], 0)
  dataType(firstYear) <- "INT2S"
  
  # Save last (years) raster tiles to disk with a randomly generated name
  last <- names(out)[length(out)]
  lastYear <- out[[last]] # left the code like this because it is easier to name the rasters in case I need to save them to disk
  lastYear[] <- round(lastYear[], 0)
  dataType(lastYear) <- "INT2S"

  #Stack rasters in the list
  predictedStack <- raster::stack(out)
  arrayStack <- raster::as.array(predictedStack)
 
  message(crayon::yellow(paste0("Extracting results from slope of predictions for ", spName)))
  
  # Slope rasters
  slopeCoefficientVal <- apply(X = arrayStack, MARGIN = c(1, 2), FUN = function(x){ # Need to extract 2 values from this lapply... think about how to do it!
      slpCoef <- RcppArmadillo::fastLmPure(X = cbind(1, times), y = x) # Original formula was way slower: lm(x ~ times, data = dfX,  na.action = na.omit)
       coef <- slpCoef$coefficients[2]
       pVal <- 2*pt(abs(slpCoef$coefficients/slpCoef$stderr), slpCoef$df.residual, lower.tail=FALSE)[2]
      pValList <- list()
      pValList <- pVal
  return(coef)
  })
  
  browser() # Just need to fix mod to become 2 rasters coef and sig
  
  slopeCoefficient <- predictedStack[[1]] %>%
    raster::setValues(slopeCoefficientVal) # restart Here!!
  names(slopeCoefficient) <- "slopeCoefficient"
  slopeCoefficient[] <- slopeCoefficient[]*1000
  slopeCoefficient[] <- round(slopeCoefficient[], 0)
  dataType(slopeCoefficient) <- "INT2S"
  # 
  # # Signifficancy of coefficient's raster
  # slopeSignificancyVal <- apply(X = arrayStack, MARGIN = c(1,2), FUN = function(x){
  #   dfX <- data.frame(x, times)
  #   mod <- tryCatch({
  #     slpCoef <- lm(x ~ times, data = dfX,  na.action = na.omit)
  #     coef <- coef(summary(slpCoef))["times","Pr(>|t|)"]
  #   },
  #   error = function(e) {
  #     NA
  #   })
  #   return(mod)
  # })
  # 
  # slopeSignificancy <- predictedStack[[1]] %>%
  #   raster::setValues(slopeSignificancyVal)
  # names(slopeSignificancy) <- "slopeSignificancy"
  # slopeSignificancy[] <- slopeSignificancy[]*1000
  # slopeSignificancy[] <- round(slopeSignificancy[], 0)
  # dataType(slopeSignificancy) <- "INT2S"

  modPredict <- list(firstYear, lastYear, slopeSignificancy, slopeCoefficient)
  names(modPredict) <- c("firstYear", "lastYear", "slopeSignificancy", "slopeCoefficient")

  rm(out, firstYear, lastYear, slopeSignificancy, slopeCoefficient)
  invisible(gc())

  return(suppressWarnings(modPredict))
}
