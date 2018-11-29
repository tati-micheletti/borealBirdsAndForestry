maskingSpeciesRange <- function(densityRasters = spRas,
                      sp = sp, pathData = pathData,
                      studyArea = studyArea,
                      filenameRas = densityFiles[[sp]],
                      birdsRangeList = birdsRangeList){
  
      pathHR <- file.path(pathData, "maskedHR")
      suppressWarnings(dir.create(pathHR))
      spRangePath <- birdsRangeList[[sp]]
      spRange <- prepInputs(url = spRangePath,
                            targetFile = paste0(sp, "_corecurrmeanBSI.asc"),
                            destinationPath = pathHR, fun = raster)
      crs(spRange) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs" #Had to "guess" the crs, its not in the original files!
      spRange <- postProcess(spRange, studyArea = studyArea, format = "GTiff", datatype = "INT1U")
      vals <- raster::getValues(x = spRange)
      vals[vals < 1] <- NA # Masking to ones. Binary raster
      spRange <- raster::setValues(x = spRange, values = vals)
      rm(vals)
      invisible(gc())
      spRange <- projectInputs(spRange, targetCRS = crs(densityRasters))# Need to reproject the SpRange as I want the same projection as the original trends raster in the end
      suppressWarnings(reproducible::postProcess(x = densityRasters, rasterToMatch = spRange,
                                                 maskWithRTM = TRUE, format = "GTiff", overwrite = TRUE, 
                                                 filename2 = filenameRas))
      invisible(gc())
  return(filenameRas)
}