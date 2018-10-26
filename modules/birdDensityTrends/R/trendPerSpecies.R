trendPerSpecies <- function(birdSpecies = sim$birdSpecies,
                            predictRas = sim$predictRas,
                            startTime = start(sim),
                            endTime = end(sim),
                            outPath = outputPath(sim)){
  
if (!identical(names(predictRas[[1]]), birdSpecies)){
  stop("Your species list and predicted rasters do not match. Please revise the code")
}
  # Select each species' time series
  trends <- lapply(X = 1:length(birdSpecies), FUN = function(sp){
    # message(crayon::yellow(paste0("Extracting results from first and last",
    #                               " years of predictions for ", birdSpecies[sp])))
    birdTS <- lapply(predictRas, `[[`, birdSpecies[sp])
    # firstYear <- birdTS[[1]] # We are not saving the first and last years, at least for now...
    # lastYear <- birdTS[[length(birdTS)]]
     message(crayon::yellow(paste0("Extracting results from slope of predictions for ", birdSpecies[sp])))
     message(crayon::yellow(paste0("This is species ", sp, " of a total of ", length(birdSpecies),
                                   " species.")))
     message(crayon::blurred(paste0("This might take some time... (Time: ", Sys.time(), ")")))
    predictedStack <- raster::stack(birdTS[])
    arrayStack <- Cache(raster::as.array, x = predictedStack, #/1000 # We are not yet multiplying the 
                        userTags = paste0("arrayStack", birdSpecies[sp]))
    # Slope rasters: outputs a list of coefficient and p value
    times <- c(startTime:endTime)
    slopeValues <- apply(X = arrayStack, MARGIN = c(1, 2), FUN = function(x){
      slpCoef <- RcppArmadillo::fastLmPure(X = cbind(1, times), y = x) # Original formula was way slower: lm(x ~ times, data = dfX,  na.action = na.omit)
      #, userTags = paste0("slopeValues", birdSpecies[sp])
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
    
    # Assigning significancy values to the raster, multiplying by 1000 for efficient storage
    slopeSignificancy <- predictedStack[[1]] %>%
      raster::setValues(slopeSignificancyVals)
    names(slopeSignificancy) <- "slopeSignificancy"
    
    # Assigning coefficient values to the raster, multiplying by 1000 for efficient storage
    slopeCoefficient <- predictedStack[[1]] %>%
      raster::setValues(slopeCoefficientVals)
    names(slopeCoefficient) <- "slopeCoefficient"
    # Now we are excluding the significancy of trends
    # that are in fact 0 (which return some 
    # non-signifficant values of significancy that 
    # we don't want to plot)
    slopeSignificancy[slopeCoefficient == 0] <- 1
    vals <- getValues(x = slopeSignificancy)
    vals[vals < 0.05] <- -999
    vals[vals >= 0.05] <- 0 # Rasters with value 0 in THIS GRAPH are NOT signifficant (the original saved graphs have the real significancy)
    vals[vals == -999] <- 1 # Rasters with value 1 in THIS GRAPH are signifficant (the original saved graphs have the real significancy)
    slopeSignificancy <- setValues(slopeSignificancy, vals)
    storage.mode(slopeSignificancy[]) <- "integer"
        message(crayon::yellow(paste0("Masking significant pixels for ", birdSpecies[sp], 
                                  " (Time: ", Sys.time(), ")")))
    
    if (!identical(raster::extent(slopeCoefficient), raster::extent(slopeSignificancy))){
      if (raster::ncell(slopeSignificancy) < raster::ncell(slopeCoefficient)) {
        slopeCoefficient <- raster::crop(x = slopeCoefficient, y = slopeSignificancy)
      }
      if (raster::ncell(slopeSignificancy) > raster::ncell(slopeCoefficient)) {
        slopeSignificancy <- raster::crop(x = slopeSignificancy, y = slopeCoefficient)
      }
      
      raster::extent(slopeCoefficient) <- raster::alignExtent(extent = raster::extent(slopeCoefficient), 
                                                     object = slopeSignificancy, 
                                                     snap = "near")
    }
    slopePath <- file.path(outPath, paste0("slopeRas", birdSpecies[sp], ".tif"))
    Cache(raster::mask, x = slopeCoefficient, mask = slopeSignificancy,
                     maskvalue = 0, updatevalue = 0,
                     filename = slopePath, overwrite = TRUE,
                     userTags = paste0("maskedSlope", birdSpecies[sp]))
    rm(slopeSignificancy)
    rm(slopeCoefficient)
    gc()
    return(slopePath)
  })
names(trends) <- birdSpecies
  return(trends)  
}
