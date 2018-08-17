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
    #Make a stack of the density area in a single year and the expected abundance. Use to predict with fitModel   

  out <- lapply(names(stackDistances), function(x){ # Single tile with all years
    tileYear <- x
    q <- stackDistances[[x]]
    names(birdDensityRasters) <- 'density'
    tempStack <- stack(q, birdDensityRasters)
    if(spName == "BLPW"){
    }
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
  
  # Save last (years) raster tiles to disk with a randomly generated name
  last <- names(out)[length(out)]
  lastYear <- out[[last]] # left the code like this because it is easier to name the rasters in case I need to save them to disk

  #Stack rasters in the list
  predictedStack <- raster::stack(out[])
  arrayStack <- raster::as.array(predictedStack/1000)

  message(crayon::yellow(paste0("Extracting results from slope of predictions for ", spName)))

  # Slope rasters: outputs a list of coefficient and p value
  slopeValues <- apply(X = arrayStack, MARGIN = c(1, 2), FUN = function(x){
      slpCoef <- RcppArmadillo::fastLmPure(X = cbind(1, times), y = x) # Original formula was way slower: lm(x ~ times, data = dfX,  na.action = na.omit)
       coef <- slpCoef$coefficients[2]
       pVal <- 2*pt(abs(slpCoef$coefficients/slpCoef$stderr), slpCoef$df.residual, lower.tail=FALSE)[2]
  return(list(coef = coef, pVal = pVal))
  })

  slopeCoefficientVals <- matrix(unlist(lapply(slopeValues, `[[`, 1)), 
                                 nrow = nrow(arrayStack), 
                                 ncol = ncol(arrayStack), 
                                 byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix
  
  slopeSignificancyVals <- matrix(unlist(lapply(slopeValues, `[[`, 2)), 
                                  nrow = nrow(arrayStack), 
                                  ncol = ncol(arrayStack), 
                                  byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix

  # Assigning coefficient values to the raster, multiplying by 1000 for efficient storage
  slopeCoefficient <- predictedStack[[1]] %>%
    raster::setValues(slopeCoefficientVals)
  names(slopeCoefficient) <- "slopeCoefficient"
  slopeCoefficient[] <- slopeCoefficient[]*1000
  storage.mode(slopeCoefficient[]) <- "integer"

  # Assigning significancy values to the raster, multiplying by 1000 for efficient storage
  slopeSignificancy <- predictedStack[[1]] %>%
    raster::setValues(slopeSignificancyVals)
  names(slopeSignificancy) <- "slopeSignificancy"
  slopeSignificancy[slopeCoefficient == 0] <- 1 # We are excluding the significancy of trends 
                                                # that are in fact 0 (which return some 
                                                # non-signifficant values of significancy that 
                                                # we don't want to plot)
  slopeSignificancy[] <- slopeSignificancy[]*1000
  storage.mode(slopeSignificancy[]) <- "integer"
  
  # Building prediction list
  modPredict <- list(firstYear, lastYear, slopeSignificancy, slopeCoefficient)
  names(modPredict) <- c("firstYear", "lastYear", "slopeSignificancy", "slopeCoefficient")

  return(suppressWarnings(modPredict))
}
