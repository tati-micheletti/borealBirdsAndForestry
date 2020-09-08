prepareDTforPrediction <- function(currentTime,
                                 pathData,
                                 focalRasters,
                                 birdDensityRasters,
                                 overwriteBirdDensityDT = FALSE){
  
  # Assertion: rasters align
  testthat::expect_equal(res(focalRasters), res(birdDensityRasters))
  testthat::expect_equal(ncell(focalRasters), ncell(birdDensityRasters))
  raster::extent(birdDensityRasters) <- raster::alignExtent(extent = raster::extent(birdDensityRasters),
                                                            object = focalRasters,
                                                            snap = "near")
  # This might be happening as we are using layers created with a previous version of 
  # reproducible, which had problems with aligning layers. One option is to snap:
  tryCatch(raster::stack(birdDensityRasters, focalRasters), error = function(e){
    stop(paste0("For some wicked reason these birdDensityRasters and focalRasters ",
                "are not aligning even after snapping. Please debug."))
  })
  
  # Build the predictive dt: birdDensity
  densityDT <- lapply(X = names(birdDensityRasters), 
                      FUN = function(spName){
                        densityTableName <- file.path(Paths$inputPath, 
                                                      paste0("densityDT_", spName, ".qs"))
                        if (any(!file.exists(densityTableName), 
                                isTRUE(overwriteBirdDensityDT))){
                          message(paste0("Building the predictive",
                                         " data frame for ", crayon::yellow(spName)))
                          ras <- birdDensityRasters[[spName]]                   
                          rasValues <- data.table::data.table(pixelID = 1:ncell(ras), 
                                                              raster::getValues(ras))
                          names(rasValues) <- c("pixelID", names(ras))
                          rasValues <- na.omit(rasValues)
                          qsave(rasValues, densityTableName)
                        } else {
                          message(crayon::green(paste0("Density data.table exists for ",
                                         spName, ". Returning.")))
                          rasValues <- qread(densityTableName)
                        }
                        # log density
                        rasValues[, paste0("log", spName) := log(get(spName))][, (spName) := NULL]
                        return(rasValues)
                      })
  names(densityDT) <- names(birdDensityRasters)
  
  # Build the predictive dt: disturbance
  envNA <- new.env(parent = emptyenv())
  disturbanceDT <- lapply(X = names(focalRasters), 
                      FUN = function(rasName){
                        ras <- focalRasters[[rasName]]                   
                        rasValues <- data.table::data.table(pixelID = 1:ncell(ras), 
                                                            raster::getValues(ras))
                        names(rasValues) <- c("pixelID", rasName)
                        message(paste0("Predictive",
                                       " data frame built for ", crayon::yellow(rasName)))
                        rasValues <- na.omit(rasValues)
                        return(rasValues)
                      })
  
  invisible(lapply(disturbanceDT, function(i) setkey(i, pixelID)))
  disturbanceDT <- Reduce(function(...) merge(...), disturbanceDT)
  
  # Merge datasets
  mergedDT <- lapply(names(densityDT), function(species){
    DT <- densityDT[[species]]
    assign(x = "maxNROW", value = NROW(DT), 
           envir = envNA)
    # We might have some NA's in the P_500 and P_100 columns
    # For each species:
    assign(species, NROW(DT), envir = envNA)
    setkey(DT, pixelID)
    mergedDT <- merge(DT, disturbanceDT)
    assign(x = "effNROW", value = NROW(mergedDT), 
           envir = envNA)
    message(paste0("Total amount of pixels removed due to NA in focal rasters: ", 
                   round((1-(get("effNROW", envir = envNA)/get("maxNROW", envir = envNA)))*100, 1), "%",
                   "\n These might be due to non-forested places and/or burned areas and/or areas with development"))
    return(mergedDT)
  })
  names(mergedDT) <- names(densityDT)

  message(paste0("Finished tables for prediction ",
                 " for year ", crayon::green(currentTime), 
                 "(Time:", Sys.time(), ")"))
  
  return(mergedDT)
}
