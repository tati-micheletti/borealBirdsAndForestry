applyFocalToTiles <- function(
  listTilePaths = sim$rastersList,
  pathCache = cachePath(sim),
  pathData = dataPath(sim),
  forestClass = P(sim)$forestClass,
  focalDistance = P(sim)$focalDistance,
  disturbanceClass = P(sim)$disturbanceClass,
  recoverTime = P(sim)$recoverTime,
  useParallel = P(sim)$useParallel,
  resampledRes = P(sim)$resampledRes,
  currentYear = time(sim),
  nNodes = P(sim)$nNodes){
  
  # Subset matching tiles
  totalTiles <- unique(lengths(listTilePaths))
  lengthVector <- 1:totalTiles
  orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
    sbset <- unlist(lapply(listTilePaths, `[[`, index), use.names = FALSE)
    return(sbset)
  })
  message(crayon::green("Tiles organized..."))
  
  names(orderedRasterList) <- paste0("tile", lengthVector)
  
  message(crayon::red(paste0("Starting focal operations for year ",
                             currentYear, " (Time: "
                             , Sys.time(), ")")))
  invisible(checkPath(file.path(pathData, "resampled"), create = TRUE))
  mergedTilesName <- file.path(pathData, paste0("mergedFocal", currentYear, "-", max(focalDistance), "Res", resampledRes, "m.tif"))
  if (file.exists(mergedTilesName)) {
    message(crayon::green(paste0("Year ",
                                 currentYear, " has already been processed and is being loaded.",
                                 " Skipping to next year...")))
    
    return(raster::raster(mergedTilesName))
    
  } else {    # Here we check for parallel option
    
    if (!length(useParallel) == 0){
      if (useParallel == "across"){ # then do "local" then NULL
        message(crayon::red(paste0("Paralellizing tiles ACROSS MACHINES Messages will be suppressed until operation is complete")))
        hosts <- c(rep("132.156.148.172", 10), rep("132.156.148.171", 10), rep("132.156.149.44", 10), rep("localhost", 10))
        cl <- parallel::makePSOCKcluster(hosts, 
                                         outfile = file.path(pathCache, "logParallel"), 
                                         master = "10.20.0.68")
        pkg <- (.packages())
        clusterExport(cl = cl, varlist = ls(), envir = environment())
        loadAllPackagesInCluster <- function(reproducible = "reproducible", pakages = pkg){
          tryCatch(require(reproducible), error = devtools::install_github("PredictiveEcology/SpaDES.core", ref = "development"))
          reproducible::Require(pkg)
        }
        clusterCall(cl = cl, fun = loadAllPackagesInCluster)

        focalTilesToMerge <- parallel::clusterApplyLB(x = lengthVector, fun = focalToTiles,
                                                      totalTiles = totalTiles,
                                                      cl = cl,
                                                      orderedRasterList = orderedRasterList,
                                                      pathData = pathData,
                                                      forestClass = forestClass,
                                                      focalDistance = focalDistance,
                                                      disturbanceClass = disturbanceClass,
                                                      recoverTime = recoverTime,
                                                      resampledRes = resampledRes,
                                                      currentYear = currentYear)
        parallel::stopCluster(cl)
        
      } else { # Outlist is a list of all the tiles after running the predictions
        if (useParallel == "local"){
          message(crayon::red(paste0("Paralellizing tiles LOCALLY. Messages will be suppressed until operation is complete")))
          maxCl <- nNodes # ifelse(length(lengthVector) < nNodes, length(lengthVector), nNodes)
          cl <- parallel::makeForkCluster(maxCl, outfile = file.path(pathCache, "logParallelFocal"))
          
          # Entering each tile group
          focalTilesToMerge <- parallel::clusterApplyLB(x = lengthVector, fun = focalToTiles,
                                      cl = cl,
                                      orderedRasterList = orderedRasterList,
                                      totalTiles = totalTiles,
                                      pathData = pathData,
                                      forestClass = forestClass,
                                      focalDistance = focalDistance,
                                      disturbanceClass = disturbanceClass,
                                      recoverTime = recoverTime,
                                      resampledRes = resampledRes,
                                      currentYear = currentYear)
          
          parallel::stopCluster(cl)
          
        } else {
          stop("Provide argument to useParallel: 'across', 'local' or NULL")
        }
      }
    } else {
      
      # Entering each tile group
      focalTilesToMerge <- lapply(X = lengthVector, FUN = focalToTiles, 
                                  totalTiles = totalTiles,
                                  orderedRasterList = orderedRasterList,
                                  pathData = pathData,
                                  forestClass = forestClass,
                                  focalDistance = focalDistance,
                                  disturbanceClass = disturbanceClass,
                                  recoverTime = recoverTime,
                                  resampledRes = resampledRes,
                                  currentYear = currentYear)
      
    }
    gc()
    # mergedFocalTiles <- SpaDES.tools::mergeRaster(focalTilesToMerge) # When mergeRaster is fixed for the user to pass the function for mosaic (max)
    # function (max instead of mean, passable as argument) DELETE NEXT LINE UNTIL 253
    focalTilesToMerge <- lapply(X = focalTilesToMerge, FUN = function(ras){
      rTemplate <- focalTilesToMerge[[1]]
      raster::extent(ras) <- raster::alignExtent(extent = raster::extent(ras),
                                                    object = rTemplate,
                                                    snap = "near")
      return(ras)
    })
    rasMosaicArgs <- focalTilesToMerge
    rasMosaicArgs$fun <- max
    mergedFocalTiles <- do.call(what = raster::mosaic, args = rasMosaicArgs)
    rm(focalTilesToMerge)
    rm(rasMosaicArgs)
    gc()
    mergedFocalTiles <- round(x = mergedFocalTiles, digits = 4)
    raster::writeRaster(x = mergedFocalTiles, filename = mergedTilesName, overwrite = TRUE, format = "GTiff")
    rm(mergedFocalTiles)
    gc()
    return(raster::raster(mergedTilesName))
   } 
}