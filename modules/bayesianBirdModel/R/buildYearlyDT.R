buildYearlyDT <- function(currentTime,
                          pathData,
                          rP,
                          ageMap,
                          fixedDT,
                          focalRasters,
                          birdDensityRasters){
  
  message(paste0("Building the predictive",
                 " data frame for ", crayon::green(names(birdDensityRasters))))
  
  # ageMap = correctedAge
  ageRaster <- postProcess(x = ageMap, studyArea = rP, 
                           filename2 = NULL) - (2004 - currentTime)
  ageRaster[ageRaster < 0] <- 0
  names(ageRaster) <- "correctedAge"
  
  # birdDensityRasters = logDENSITY_BIRD
  density <- log(postProcess(x = birdDensityRasters, studyArea = rP, 
                         filename2 = NULL, rasterToMatch = ageRaster))
  names(density) <- paste0("logDENSITY_", names(birdDensityRasters))
  
  # focalRasters = State_P_XXX
  env <- environment()
  focalNames <- grepMulti(x = names(focalRasters), patterns = currentTime)
  invisible(lapply(X = focalNames, function(ras){
    fc <- postProcess(x = focalRasters[[ras]], studyArea = rP, 
                      filename2 = NULL, rasterToMatch = ageRaster)
    names(fc) <- if (grepl(names(focalRasters[[ras]]), pattern = "100")) "State_P_100" else "State_P_500"
    assign(names(fc), fc, envir = env)
  }))

  # Build the predictive dt
  predTable <- lapply(X = c("density", "ageRaster", "State_P_100", "State_P_500"), 
                      FUN = function(ras){
    ras <- get(ras)                    
    rasValues <- data.table::data.table(ID = 1:ncell(ras), 
                                            raster::getValues(ras))
    names(rasValues) <- c("ID", names(ras))
    return(rasValues)
  })

  invisible(lapply(predTable, function(i) setkey(i, ID)))
  mergedPredTable <- Reduce(function(...) merge(..., all = TRUE), predTable)
  
  # Remove from the table all NA's in density raster. We shouldn't anyway predic for those!
  mergedPredTable <- na.omit(mergedPredTable, cols = names(density))
  
  # Add X and Y coordinates === Not that I need it I guess... Maybe should take this out!
  XY <- data.table(mergedPredTable$ID, raster::xyFromCell(object = density, cell = mergedPredTable$ID))
  names(XY) <- c("ID", "X", "Y")
  mergedPredTable <- merge(XY, mergedPredTable, by = "ID")
  
  # Now add the currentYear
  mergedPredTable$YYYY <- currentTime
  
  # Now rbindlist with fixedDT (statistical + predictive tables)
  diffNames <- setdiff(names(fixedDT), names(mergedPredTable))
  mergedPredTable[, c(diffNames) := list(NA)]
  diffNames <- setdiff(names(mergedPredTable), names(fixedDT))
  fixedDT[, c(diffNames) := list(NA)]
  
  order <- match(names(mergedPredTable), names(fixedDT))
  setcolorder(fixedDT, order)
  # finalTable <- rbind(fixedDT, mergedPredTable)
  return(list(predictionData = mergedPredTable, originalData = fixedDT))
}

