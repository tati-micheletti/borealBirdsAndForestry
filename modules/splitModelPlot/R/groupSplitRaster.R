groupSplitRaster <- function(models = models, # This is already lapplying though each species
                             birdDensityRasters = birdDensityRasters, #Single Raster inherited from models at 4000m resolution for the specific species
                             disturbanceType = disturbanceType, # File path
                             disturbanceYear = disturbanceYear, # File path
                             landCover = landCover, # File path
                             pathData = pathData,
                             nx = nx,
                             ny = ny,
                             buffer = buffer,
                             rType = rType,
                             startTime = startTime,
                             endTime = endTime,
                             forestClass = forestClass,
                             focalDistance = focalDistance,
                             disturbanceClass = disturbanceClass,
                             intermPath = intermPath,
                             rP = rP,
                             recoverTime = recoverTime,
                             useParallel = useParallel,
                             extractFrom4kRasters = extractFrom4kRasters) {
  
  # ======================== STARTED GIS ===================================
  
  if (extractFrom4kRasters == TRUE){
    birdDensityRastersPath <- birdDensityRasters
    birdDensityRasters <- raster::raster(birdDensityRasters)
    spName <- substring(birdDensityRasters@data@names, 8) # [ FIX ] Need to move it to the end... Otherwise will do the operations at 30m for no reason? Needs to be checked if it is working properly!
  } else {
    birdDensityRastersPath <- birdDensityRasters
    birdDensityRasters <- raster::raster(birdDensityRasters)
    spName <- substring(birdDensityRasters@data@names, 8)
  }
  
  message(crayon::green(paste0("Initializing GIS operations for ", spName)))
  birdDensityResampled <- file.path(pathData, "birdDensityResampled", spName, ".tif")

  if (!all(raster::res(birdDensityRasters) == c(30, 30))){
    if (!file.exists(birdDensityResampled)) {
      
      # Bring original density raster to memory, and make it smaller, rewrite it
      birdDensityRasters[] <- birdDensityRasters[]
      birdDensityRasters[] <- round(birdDensityRasters[]*1000, 0)
      storage.mode(birdDensityRasters[]) = "integer"
      raster::writeRaster(x = birdDensityRasters, filename = birdDensityRastersPath, overwrite = TRUE)
      
      # Resample density rasters
      templateRaster <- raster(resolution = c(30, 30),
                               crs = raster::crs(birdDensityRasters),
                               ext = extent(birdDensityRasters))
      
      crsTemplate <- as.character(raster::crs(templateRaster))
      rP <- sf::st_as_sf(rP)
      if (file.exists(file.path(pathData, "rP_sf.shp"))) file.remove(file.path(pathData, "rP_sf.shp"))
      sf::st_write(obj = rP, dsn = file.path(pathData, "rP_sf.shp"))
      message(crayon::yellow(paste0("Resampling density rasters to 30m resolution for ", spName)))
      reproducible::Cache(gdalwarp, srcfile = birdDensityRastersPath, 
               dstfile = birdDensityResampled, 
               overwrite = TRUE,
               cutline = file.path(pathData, "rP_sf.shp"),
               dstalpha = TRUE, 
               s_srs = crsTemplate,
               t_srs = crsTemplate,
               multi = TRUE, 
               of = "GTiff",
               crop_to_cutline = TRUE, 
               tr = c(30, 30))
      birdDensityRasters <- raster::raster(birdDensityResampled)
      gc()
    } else {
      birdDensityRasters <- raster::raster(birdDensityResampled)
      gc()
    }
  }
  
  # ~~~~~~~~~ LANDCOVER ~~~~~~~~~~~~
  # Load landCover
  browser() # Land cover prepInputs not working for area similar to NWT (when reprojecting "CAN_NALCMS_LC_30m_LAEA_mmu12_urb05.tif")
  landCover <- Cache(prepInputs, targetFile = file.path(pathData, "CAN_NALCMS_LC_30m_LAEA_mmu12_urb05.tif"),
                          destinationPath = pathData,
                          rasterToMatch = birdDensityRasters,
                          studyArea = rP, userTags = "objectName::landCover")
  
  landCover[] <- round(landCover[], 0)
  storage.mode(landCover[]) = "integer"
  
  # Split landCover
  message(crayon::yellow(paste0("Splitting lanCover tiles for ", spName)))
  landCover <- Cache(splitRaster, r = landCover, nx = nx, ny = ny, buffer = buffer,  # Splitting landCover Raster, write to disk,
                     rType = rType, path = file.path(intermPath, "land")) # override the original in memory
  gc()
  # ~~~~~~~~~ DISTURBANCE TYPE ~~~~~~~~~~~~
  
  # Load disturbanceType
  disturbanceType <- prepInputs(targetFile = file.path(pathData, "C2C_change_type.tif"), # If this is not wrking, might be becuse these objects were in sim and now they are not
                                destinationPath = pathData,
                                # rasterToMatch = birdDensityRasters,
                                studyArea = rP,
                                length = TRUE)
  disturbanceType[] <- round(disturbanceType[], 0)
  storage.mode(disturbanceType[]) = "integer"
  # Split disturbanceType
  message(crayon::yellow(paste0("Splitting disturbanceType tiles for ", spName)))
  disturbanceType <- Cache(splitRaster, r = disturbanceType, nx = nx, ny = ny, buffer = buffer,  # Splitting disturbanceType Raster, write to disk,
                           rType = rType, path = file.path(intermPath, "distType")) # override the original in memory
  gc()
  # ~~~~~~~~~ DISTURBANCE YEAR ~~~~~~~~~~~~
  
  # Load disturbanceYear
  disturbanceYear <- prepInputs(targetFile = file.path(pathData, "C2C_change_year.tif"),
                                destinationPath = pathData,
                                # rasterToMatch = birdDensityRasters,
                                studyArea = rP,
                                quick = TRUE) # Keep the file in memory only if the file is small enough.
  disturbanceYear[] <- round(disturbanceYear[], 0)
  storage.mode(disturbanceYear[]) = "integer"
  
  # Split disturbanceYear
  message(crayon::yellow(paste0("Splitting disturbanceYear tiles for ", spName)))
  disturbanceYear <- Cache(splitRaster, r = disturbanceYear, nx = nx, ny = ny, buffer = buffer,  # Splitting disturbanceYear Raster, write to disk,
                           rType = rType, path = file.path(intermPath, "distYear")) # override the original in memory
  gc()
  # ============ SPLIT ABUNDANCE ================
  # Split abundance raster
  message(crayon::yellow(paste0("Splitting density tiles for ", spName)))
  birdDensityRasters <- splitRaster(r = birdDensityRasters, nx = nx, ny = ny, buffer = buffer, rType = "INT2S") # Splitting density Raster, stays in memory, override the original
  gc()
  
  # Tile's list
  newlist <- list("distType" = disturbanceType, "distYear" = disturbanceYear, "land" = landCover, "birdDensityRasters" = birdDensityRasters) # Splitted rasters' list
  origNames <- c("disturbanceType", "disturbanceYear",
                 "landCover", "birdDensityRasters")
  
  message(crayon::green(paste0("GIS operations finalized for ", spName, "! :D")))
  
  # ======================== FINISHED GIS ===================================
  browser()
  
  lengthvect <- 1:(nx * ny)
  
  # Using parallel: Outlist is a list of all the tiles after running the predictions
  if (!length(useParallel) == 0){
    if (useParallel == "across"){ # then do "local" then NULL
      message(crayon::red(paste0("Paralellizing tiles ACROSS MACHINES Messages will be suppressed until operation is complete")))
      hosts <- c(rep("132.156.148.172", 10), rep("132.156.148.171", 10), rep("localhost", 10))
      cl <- parallel::makePSOCKcluster(hosts, 
                                       # outfile = "/mnt/storage/borealBirdsAndForestry/cache/logParallel", 
                                       master = "132.156.149.44")
      pkg <- (.packages())
      clusterExport(cl = cl, varlist = ls(), envir = environment())
      loadAllPackagesInCluster <- function(reproducible = "reproducible", pakages = pkg){
        tryCatch(require(reproducible), error = devtools::install_github("PredictiveEcology/SpaDES.core", ref = "development"))
        reproducible::Require(pkg)
      }
      clusterCall(cl = cl, fun = loadAllPackagesInCluster)
      
      outList <- parallel::clusterApplyLB(x = lengthvect,
                                          fun = tileReorder,
                                          cl = cl,
                                          spName = spName,
                                          inList = newlist,
                                          origList = origNames,
                                          startTime = startTime,
                                          endTime = endTime,
                                          forestClass = forestClass,
                                          focalDistance = focalDistance,
                                          disturbanceClass = disturbanceClass,
                                          passedModel = models,
                                          pathData = pathData,
                                          intermPath = intermPath,
                                          recoverTime = recoverTime,
                                          maxTile = length(lengthvect))
      parallel::stopCluster(cl)
      
    } else { # Outlist is a list of all the tiles after running the predictions
      if (useParallel == "local"){
        message(crayon::red(paste0("Paralellizing tiles LOCALLY. Messages will be suppressed until operation is complete")))
        cl <- parallel::makeForkCluster(10, outfile = "/mnt/storage/borealBirdsAndForestry/cache/logParallel")
        outList <- parallel::clusterApplyLB(x = lengthvect, 
                                            fun = tileReorder,
                                            cl = cl,
                                            spName = spName,
                                            inList = newlist,
                                            origList = origNames,
                                            startTime = startTime,
                                            endTime = endTime,
                                            forestClass = forestClass,
                                            focalDistance = focalDistance,
                                            disturbanceClass = disturbanceClass,
                                            passedModel = models,
                                            pathData = pathData,
                                            intermPath = intermPath,
                                            recoverTime = recoverTime,
                                            maxTile = length(lengthvect))
        parallel::stopCluster(cl)
        
      } else {
        stop("Provide argument to useParallel: 'across', 'local' or NULL")
      }
    }
  } else {
    
    # Not using parallel: Outlist is a list of all the tiles after running the predictions
    outList <- lapply(X = lengthvect,
                      FUN = tileReorder,
                      spName = spName,
                      inList = newlist,
                      origList = origNames,
                      startTime = startTime,
                      endTime = endTime,
                      forestClass = forestClass,
                      focalDistance = focalDistance,
                      disturbanceClass = disturbanceClass,
                      passedModel = models,
                      pathData = pathData,
                      intermPath = intermPath,
                      recoverTime = recoverTime,
                      maxTile = length(lengthvect))
  }
  
  outList <- plyr::compact(outList)
  
  lengthResultRasters <- 1:length(outList[[1]])
  finalRasList <- lapply(lengthResultRasters, function(nRas){
    rasList <- lapply(X = outList, `[[`, nRas)
    rasName <- names(outList[[1]])[nRas]
    dir.create(file.path(intermPath, "outputRasters"), showWarnings = FALSE)
    finalRasPath <- file.path(intermPath, "outputRasters", paste0(spName, "_", rasName, "_", max(focalDistance), ".tif"))
    ras <- SpaDES.tools::mergeRaster(x = rasList) # If at some point mergeRaster writes to disk, delete next 3 lines below.
    raster::writeRaster(x = ras, filename = finalRasPath, overwrite = FALSE)
    rm(ras) # Remove raster from memory
    invisible(gc())
    return(finalRasPath)
  })
  
  names(finalRasList) <- names(outList[[1]])
  
  return(finalRasList)
}
