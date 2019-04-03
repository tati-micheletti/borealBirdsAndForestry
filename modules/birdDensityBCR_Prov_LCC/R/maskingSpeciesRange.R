maskingSpeciesRange <- function(densityRasters = spRas,
                      sp = sp, pathData = pathData,
                      studyArea = studyArea,
                      filenameRas = densityFiles[[sp]],
                      birdsRangeList = birdsRangeList){
  
      pathHR <- file.path(pathData, "maskedHR") %>%
        checkPath(create = TRUE)
      spRangePath <- birdsRangeList[[sp]]

      if (!file.exists(file.path(pathHR, paste0(sp, "_corecurrmeanBSI.asc")))){
        spRange <- prepInputs(url = spRangePath,
                              targetFile = paste0(sp, "_corecurrmeanBSI.asc"),
                              destinationPath = pathHR, fun = "raster::raster")
      } else {
        spRange <- raster::raster(file.path(pathHR, paste0(sp, "_corecurrmeanBSI.asc")))
      }
      crs(spRange) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" #Had to "guess" the crs, its not in the original files!
      if (!file.exists(file.path(pathData, paste0(sp, "unmasked.tif")))){
        spRange <- postProcess(spRange, studyArea = studyArea, format = "GTiff", datatype = "INT1U", filename2 = paste0(sp, "unmasked.tif"), 
                               destinationPath = pathData)
      } else {
        spRange <- raster::raster(file.path(pathData, paste0(sp, "unmasked.tif")))
      }
      vals <- raster::getValues(x = spRange)
      vals[vals < 1] <- NA # Masking to ones. Binary raster
      spRange <- raster::setValues(x = spRange, values = vals)
      rm(vals)
      invisible(gc())
      if (!file.exists(filenameRas)){
        spRange <- projectInputs(spRange, targetCRS = crs(densityRasters))# Need to reproject the SpRange as I want the same projection as the original trends raster in the end
        suppressWarnings(reproducible::postProcess(x = densityRasters, rasterToMatch = spRange,
                                                   maskWithRTM = TRUE, format = "GTiff", overwrite = TRUE, 
                                                   filename2 = filenameRas, useCache = FALSE)) 
      } else {
        spRange <- filenameRas
      }
      invisible(gc())
  return(filenameRas)
}