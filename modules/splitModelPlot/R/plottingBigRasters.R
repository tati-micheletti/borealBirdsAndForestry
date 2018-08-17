plottingBigRasters <- function(rasters = sim$populationTrends){

  sp <- names(rasters)
  
  rasterList <- lapply(X = sp, FUN = function(species){
    assign(x = paste0("firstYear_", species),value = raster::raster(eval(parse(text = paste0("rasters$", species, "$firstYear")))))
    assign(x = paste0("lastYear_", species),value = raster::raster(eval(parse(text = paste0("rasters$", species, "$lastYear")))))
    assign(x = paste0("slopeCoeff_", species),value = raster::raster(eval(parse(text = paste0("rasters$", species, "$slopeCoefficient")))))
    assign(x = paste0("slopeSig_", species),value = raster::raster(eval(parse(text = paste0("rasters$", species, "$slopeSignificancy")))))
    
    # Show only significant values
    stkYear <- raster::stack(get(paste0("firstYear_", species))/1000, get(paste0("lastYear_", species))/1000)
    stkSlope <- raster::stack(get(paste0("slopeCoeff_", species))/1000, get(paste0("slopeSig_", species))/1000)
    
    # Really significant rasters
    vals <- getValues(x = stkSlope[[2]])
    vals[vals < 0.05] <- -999
    vals[vals >= 0.05] <- 0 # Rasters with value 0 in THIS GRAPH are NOT signifficant (the original saved graphs have the real significancy)
    vals[vals == -999] <- 1 # Rasters with value 1 THIS GRAPH are signifficant (the original saved graphs have the real significancy)
    stkSlope[[2]] <- setValues(stkSlope[[2]], vals)
    
    # Or make a nice GGPLOT2 plot...
    quickPlot::Plot(stkSlope, col = rainbow(10), 
                    title = c(paste0("Slope Coefficients ", species), paste0("Slope Significancy ", species)),
                    zero.color = "grey")
    quickPlot::Plot(stkYear, col = rainbow(10), legendRange = c(0, 0.035),
                    title = c(paste0("1985 abundance ", species), paste0("2011 abundance ", species)))
    
  return(list(Years = stkYear, Slope = stkSlope))
  })

  names(rasterList) <- sp
return(rasterList)
}
  
