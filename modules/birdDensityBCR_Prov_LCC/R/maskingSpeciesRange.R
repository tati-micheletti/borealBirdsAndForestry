maskingSpeciesRange <- function(densityRasters = spRas,
                      sp = sp,
                      birdsRangeList = birdsRangeList){
      spRangePath <- birdsRangeList[[sp]]
      spRange <- prepInputs(url = spRangePath, 
                            targetFile = paste0(sp, "_corecurrmeanBSI.asc"),
                            destinationPath = pathHR, fun = raster)
      crs(spRange) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" #Had to "guess" the crs, its not in the original files!
      vals <- raster::getValues(x = spRange)
      vals[vals < 1] <- NA
      spRange <- raster::setValues(x = spRange, values = vals) %>%
        projectInputs(rasterToMatch = densityRasters) # Need to reproject the SpRange as I want the same projection as the original trends raster in the end
      densityRasMasked <- suppressWarnings(reproducible::postProcess(x = densityRasters, rasterToMatch = spRange, 
                                                 maskWithRTM = TRUE, format = "GTiff", overwrite = TRUE))
  return(densityRasMasked)
}