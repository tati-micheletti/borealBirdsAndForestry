# trendPerSpecies <- function(birdSpecies,
#                             focalDistance,
#                             predictRas,
#                             startTime,
#                             endTime,
#                             useParallel,
#                             nCores,
#                             outPath){
#   if (!identical(names(predictRas[[1]]), birdSpecies)){
#     stop("Your species list and predicted rasters do not match. Please revise the code")
#   }
#   
#   # Select each species' time series
#   message(crayon::red(paste0("Starting trend analysis (Time: "
#                              , Sys.time(), ")")))
#   
#   if (nCores == "auto") {
#     nCores <- pemisc::optimalClusterNum(70000, maxNumClusters = length(birdSpecies))
#   }
#   if (all(.Platform$OS.type != "windows", isTRUE(useParallel))) {
#     cl <- parallel::makeForkCluster(nCores, outfile = file.path(outPath, "logTrends")) # Tried, works, too slow
#     # cl <- parallel::makePSOCKcluster(sim$nCores, outfile = file.path(dataPath(sim), "logParallelBirdPrediction")) # Tried, also works, also slow
#     on.exit(try(parallel::stopCluster(cl), silent = TRUE))
#     if (nCores == 1)
#       cl <- NULL
#   } else {
#     cl <- NULL
#   }
#   
#   trends <- lapply(X = 1:length(birdSpecies), FUN = function(sp){
#     birdTS <- lapply(predictRas, `[[`, birdSpecies[sp])
#     message(crayon::blue(paste0("Bringing ", birdSpecies[sp]," raster stack to memory. (Time: ", Sys.time(),")")))
#     rasStack <- raster::stack(lapply(X = birdTS, FUN = function(r){
#       ras <- raster::raster(r)
#       ras[] <- ras[]
#       return(ras)
#     }))
#     message(crayon::yellow(paste0("Extracting results from slope of predictions for ", birdSpecies[sp])))
#     message(crayon::yellow(paste0("This is species ", sp, " of a total of ", length(birdSpecies),
#                                   " species. (Time: ", Sys.time(), ")")))
#     arrayStack <- raster::as.array(x = rasStack) #/1000 # We are not yet multiplying 
#     rm(rasStack)
#     gc()
#     # Slope rasters: outputs a list of coefficient and p value
#     times <- c(startTime:endTime)
#     message(crayon::yellow(paste0("Fitting regression model for ", birdSpecies[sp], 
#                                   " (Time: ", Sys.time(), ")")))
#     slopeValues <- apply(X = arrayStack, MARGIN = c(1, 2), FUN = function(x){
#       slpCoef <- RcppArmadillo::fastLmPure(X = cbind(1, times), y = x) # Original formula was way slower: lm(x ~ times, data = dfX,  na.action = na.omit)
#       coef <- slpCoef$coefficients[2]
#       pVal <- 2*pt(abs(slpCoef$coefficients/slpCoef$stderr), slpCoef$df.residual, lower.tail=FALSE)[2]
#       return(list(coef = coef, pVal = pVal))
#     })
#     
#     slopeCoefficientVals <- matrix(unlist(lapply(slopeValues, `[[`, 1)), 
#                                    nrow = nrow(arrayStack), 
#                                    ncol = ncol(arrayStack), 
#                                    byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix
#     
#     slopeSignificancyVals <- matrix(unlist(lapply(slopeValues, `[[`, 2)), 
#                                     nrow = nrow(arrayStack), 
#                                     ncol = ncol(arrayStack), 
#                                     byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix
#     rm(slopeValues)
#     rm(arrayStack)
#     gc()
#     # Assigning significancy values to the raster, multiplying by 1000 for efficient storage
#     slopeSignificancy <- predictedStack[[1]] %>%
#       raster::setValues(slopeSignificancyVals)
#     names(slopeSignificancy) <- "slopeSignificancy"
#     rm(slopeSignificancyVals)
#     gc()
#     
#     # Assigning coefficient values to the raster, multiplying by 1000 for efficient storage
#     slopeCoefficient <- predictedStack[[1]] %>%
#       raster::setValues(slopeCoefficientVals)
#     names(slopeCoefficient) <- "slopeCoefficient"
#     rm(slopeCoefficientVals)
#     rm(predictedStack)
#     gc()
#     
#     # Now we are excluding the significancy of trends
#     # that are in fact 0 (which return some 
#     # non-signifficant values of significancy that 
#     # we don't want to plot)
#     slopeSignificancy[slopeCoefficient == 0] <- 1
#     vals <- getValues(x = slopeSignificancy)
#     vals[vals < 0.05] <- -999
#     vals[vals >= 0.05] <- 0 # Rasters with value 0 in THIS GRAPH are NOT signifficant (the original saved graphs have the real significancy)
#     vals[vals == -999] <- 1 # Rasters with value 1 in THIS GRAPH are signifficant (the original saved graphs have the real significancy)
#     slopeSignificancy <- setValues(slopeSignificancy, vals)
#     storage.mode(slopeSignificancy[]) <- "integer"
#     message(crayon::yellow(paste0("Masking significant pixels for tile ", tiles,
#                                   " of ", birdSpecies[sp],
#                                   " (Time: ", Sys.time(), ")")))
#     rm(vals)
#     gc()
#     
#     if (!identical(raster::extent(slopeCoefficient), raster::extent(slopeSignificancy))){
#       if (raster::ncell(slopeSignificancy) < raster::ncell(slopeCoefficient)) {
#         slopeCoefficient <- raster::crop(x = slopeCoefficient, y = slopeSignificancy)
#       }
#       if (raster::ncell(slopeSignificancy) > raster::ncell(slopeCoefficient)) {
#         slopeSignificancy <- raster::crop(x = slopeSignificancy, y = slopeCoefficient)
#       }
#       raster::extent(slopeCoefficient) <- raster::alignExtent(extent = raster::extent(slopeCoefficient), 
#                                                               object = slopeSignificancy, 
#                                                               snap = "near")
#     }
#     raster::mask(x = slopeCoefficient, mask = slopeSignificancy,
#                  maskvalue = 0, updatevalue = 0,
#                  filename = slopePath, overwrite = TRUE)
#     rm(slopeSignificancy)
#     rm(slopeCoefficient)
#     gc()
#     return(slopePath)
#     browser()
#     # EcoGenetics::eco.theilsen :: use with rasterbrick
#     
#     
#     
#     # ~~~~~~~~~~~~~~~~~~~~ OPTION FOR BIG MACHINES
#     # nSpPerGroup <- round(length(birdSpecies)/3, 0) # [ FIX ] Make it more flexible! 
#     # birdSp1 <- birdSpecies[1:nSpPerGroup]
#     # birdSp2 <- birdSpecies[(nSpPerGroup + 1):(nSpPerGroup*2)]
#     # birdSp3 <- birdSpecies[((2*nSpPerGroup)+1):(nSpPerGroup*3)]
#     # bdList <- list(birdSp1, birdSp2, birdSp3)
#     #   allBirds <- lapply(X = birdSpecies, FUN = function(bird){
#     #     # Bring all rasters to memory or just extract a raster brick matrix
#     #     reorderedList <- lapply(X = 1:length(bird), FUN = function(sp){
#     #       birdTS <- lapply(predictRas, `[[`, bird[sp])
#     #       browser()
#     #       t1 <- Sys.time()
#     #       birdList <- lapply(X = birdTS, FUN = getValues)
#     #       names(birdList) <- paste0("Year", startTime:endTime)
#     #       birdListall <- data.table::rbindlist(data.table::transpose(birdList))
#     #       t2 <- Sys.time()
#     #       
#     #       
#     #       return()
#     #       # birdStack <- raster::stack(birdTS)
#     #       # birdMatrix <- getValues(birdStack) # Way too slow!
#     #     })
#     #     
#     #     if (!is.null(cl)){
#     #       message(crayon::red(paste0("Paralellizing for:\n", paste(sim$birdSpecies, collapse = "\n"),
#     #                                  "\nUsing ", sim$nCores, " cores \n",
#     #                                  "\nMessages will be suppressed until done")))
#     #       predictVec <- clusterApplyLB(seq_along(sim$birdSpecies),
#     #                                    cl = cl, function(index) {
#     #                                      corePrediction(bird = sim$birdSpecies[[index]],
#     #                                                     model = sim$predictModels[[index]],
#     #                                                     predictedName = predictedName[[index]],
#     #                                                     birdDensityRas = birdDensityVectors[[index]],
#     #                                                     pathData = dataPath(sim),
#     #                                                     disturbanceRas = disturbanceRasVector,
#     #                                                     currentTime = time(sim))
#     #                                    })
#     #     } else {
#     #       predictVec <- lapply(seq_along(sim$birdSpecies),
#     #                            function(index) {
#     #                              corePrediction(bird = sim$birdSpecies[[index]], 
#     #                                             model = sim$predictModels[[index]], 
#     #                                             predictedName = predictedName[[index]],
#     #                                             birdDensityRas = birdDensityVectors[[index]],
#     #                                             pathData = dataPath(sim),
#     #                                             disturbanceRas = disturbanceRasVector,
#     #                                             currentTime = time(sim))
#     #                            })
#     #     }
#     #     
#     #     # Reconvert vectors into rasters
#     #     rm(disturbanceRasVector)
#     #     rm(birdDensityVectors)
#     #     invisible(gc())
#     #     sim$predictRas[[paste0("Year", time(sim))]] <- lapply(predictVec, FUN = function(spVec){
#     #       rasName <- paste0("prediction", attributes(spVec)[["prediction"]])
#     #       birdRas <- raster(sim$birdDensityRasters[[1]]) # Using the first as a template. All should be the same.
#     #       birdRas <- raster::setValues(x = birdRas, values = as.numeric(spVec))
#     #       return(birdRas)
#     #     })
#     #     names(sim$predictRas[[paste0("Year", time(sim))]]) <- sim$birdSpecies      
#     #     rm(predictVec)
#     #     invisible(gc())
#     #     
#     #   
#     #   # SAVE THE PREDICTED RASTERS
#     #   sim$predictRas[[paste0("Year", time(sim))]] <- lapply(names(predictedName), function(bird){
#     #     writeRaster(sim$predictRas[[paste0("Year", time(sim))]][[bird]], 
#     #                 filename = predictedName[[bird]], format = "GTiff")
#     #     return(predictedName[[bird]])
#     #   })
#     #   names(sim$predictRas[[paste0("Year", time(sim))]]) <- sim$birdSpecies
#     #   
#     #   invisible(gc())
#     #   # Maybe return here the rasters?
#     # }) # end of lapply through bird species groups
#     
#     #################### ~~~~~~~~~~~~~~~~ End of guide code
#     
#     trends <- lapply(X = 1:length(birdSpecies), FUN = function(sp){
#       birdTS <- lapply(predictRas, `[[`, birdSpecies[sp])
#       
#       # Rasters are still too big to fit memory. Splitting and selecting each group:
#       suppressWarnings(dir.create(path = file.path(outPath, paste0("trends", focalDistance))))
#       mergedTilesName <- file.path(outPath, paste0("trends", focalDistance), 
#                                    paste0("mergedTrend", birdSpecies[sp], 
#                                           focalDistance, ".tif"))
#       if (file.exists(mergedTilesName)) {
#         message(crayon::green(paste0("Trend analysis for ",birdSpecies[sp], 
#                                      " has already been processed and is being loaded.",
#                                      " Skipping to next species...")))
#         return(raster::raster(mergedTilesName))
#       } else {
#         splittedPath <- file.path(outPath, paste0("trends", focalDistance))
#         message(crayon::yellow(paste0("Splitting rasters for ", birdSpecies[sp], " (Time: ", Sys.time(), ")")))
#         splittedList <- Cache(lapply, X = birdTS, splitRaster, nx = 1, ny = 1, buffer = c(800, 800),
#                               path = splittedPath, userTags = paste0("splitTrends", focalDistance, birdSpecies[sp]))
#         totalTiles <- unique(lengths(splittedList))
#         lengthVector <- 1:totalTiles
#         orderedRasterList <- lapply(X = lengthVector, FUN = function(index){
#           sbset <- unlist(lapply(splittedList, `[[`, index), use.names = FALSE)
#           return(sbset)
#         })
#         names(orderedRasterList) <- paste0("tile", lengthVector)
#         message(crayon::yellow(paste0("Extracting results from slope of predictions for ", birdSpecies[sp])))
#         message(crayon::yellow(paste0("This is species ", sp, " of a total of ", length(birdSpecies),
#                                       " species. (Time: ", Sys.time(), ")")))
#         # Entering each tile group
#         focalTilesToMerge <- lapply(X = lengthVector, FUN = function(tiles){
#           slopePath <- file.path(outPath, paste0("trends", focalDistance), paste0("slopeRas", focalDistance, 
#                                                                                   birdSpecies[sp], "tile", tiles, ".tif"))
#           if (file.exists(slopePath)) {
#             return(slopePath)
#           } else {
#             predictedStack <- raster::stack(orderedRasterList[[tiles]])
#             arrayStack <- raster::as.array(x = predictedStack) #/1000 # We are not yet multiplying 
#             
#             # Slope rasters: outputs a list of coefficient and p value
#             times <- c(startTime:endTime)
#             message(crayon::yellow(paste0("Fitting regression model for ", birdSpecies[sp], " tile ", 
#                                           tiles, " (Time: ", Sys.time(), ")")))
#             slopeValues <- apply(X = arrayStack, MARGIN = c(1, 2), FUN = function(x){
#               browser()
#               slpCoef <- RcppArmadillo::fastLmPure(X = cbind(1, times), y = x) # Original formula was way slower: lm(x ~ times, data = dfX,  na.action = na.omit)
#               coef <- slpCoef$coefficients[2]
#               pVal <- 2*pt(abs(slpCoef$coefficients/slpCoef$stderr), slpCoef$df.residual, lower.tail=FALSE)[2]
#               return(list(coef = coef, pVal = pVal))
#             })
#             slopeCoefficientVals <- matrix(unlist(lapply(slopeValues, `[[`, 1)), 
#                                            nrow = nrow(arrayStack), 
#                                            ncol = ncol(arrayStack), 
#                                            byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix
#             
#             slopeSignificancyVals <- matrix(unlist(lapply(slopeValues, `[[`, 2)), 
#                                             nrow = nrow(arrayStack), 
#                                             ncol = ncol(arrayStack), 
#                                             byrow = FALSE) # retrieves values from slope Coefficient, arranges into a corrected (inversed) matrix
#             rm(slopeValues)
#             rm(arrayStack)
#             gc()
#             # Assigning significancy values to the raster, multiplying by 1000 for efficient storage
#             slopeSignificancy <- predictedStack[[1]] %>%
#               raster::setValues(slopeSignificancyVals)
#             names(slopeSignificancy) <- "slopeSignificancy"
#             rm(slopeSignificancyVals)
#             gc()
#             
#             # Assigning coefficient values to the raster, multiplying by 1000 for efficient storage
#             slopeCoefficient <- predictedStack[[1]] %>%
#               raster::setValues(slopeCoefficientVals)
#             names(slopeCoefficient) <- "slopeCoefficient"
#             rm(slopeCoefficientVals)
#             rm(predictedStack)
#             gc()
#             
#             # Now we are excluding the significancy of trends
#             # that are in fact 0 (which return some 
#             # non-signifficant values of significancy that 
#             # we don't want to plot)
#             slopeSignificancy[slopeCoefficient == 0] <- 1
#             vals <- getValues(x = slopeSignificancy)
#             vals[vals < 0.05] <- -999
#             vals[vals >= 0.05] <- 0 # Rasters with value 0 in THIS GRAPH are NOT signifficant (the original saved graphs have the real significancy)
#             vals[vals == -999] <- 1 # Rasters with value 1 in THIS GRAPH are signifficant (the original saved graphs have the real significancy)
#             slopeSignificancy <- setValues(slopeSignificancy, vals)
#             storage.mode(slopeSignificancy[]) <- "integer"
#             message(crayon::yellow(paste0("Masking significant pixels for tile ", tiles,
#                                           " of ", birdSpecies[sp],
#                                           " (Time: ", Sys.time(), ")")))
#             rm(vals)
#             gc()
#             
#             if (!identical(raster::extent(slopeCoefficient), raster::extent(slopeSignificancy))){
#               if (raster::ncell(slopeSignificancy) < raster::ncell(slopeCoefficient)) {
#                 slopeCoefficient <- raster::crop(x = slopeCoefficient, y = slopeSignificancy)
#               }
#               if (raster::ncell(slopeSignificancy) > raster::ncell(slopeCoefficient)) {
#                 slopeSignificancy <- raster::crop(x = slopeSignificancy, y = slopeCoefficient)
#               }
#               raster::extent(slopeCoefficient) <- raster::alignExtent(extent = raster::extent(slopeCoefficient), 
#                                                                       object = slopeSignificancy, 
#                                                                       snap = "near")
#             }
#             raster::mask(x = slopeCoefficient, mask = slopeSignificancy,
#                          maskvalue = 0, updatevalue = 0,
#                          filename = slopePath, overwrite = TRUE)
#             rm(slopeSignificancy)
#             rm(slopeCoefficient)
#             gc()
#             return(slopePath)
#           }
#         })
#         gc()
#         message(crayon::green(paste0("Merging tiles for ", birdSpecies[sp], 
#                                      " (Time: ", Sys.time(), ")")))
#         focalTilesToMerge <- lapply(X = focalTilesToMerge, FUN = raster)
#         rasMosaicArgs <- focalTilesToMerge
#         rasMosaicArgs$fun <- max
#         mergedFocalTiles <- do.call(what = raster::mosaic, args = rasMosaicArgs)
#         # mergedFocalTiles <- SpaDES.tools::mergeRaster(focalTilesToMerge) # We can use this back when 
#         rm(focalTilesToMerge)
#         rm(rasMosaicArgs)
#         gc()
#         raster::writeRaster(x = mergedFocalTiles, filename = mergedTilesName, 
#                             overwrite = TRUE, format = "GTiff")
#         rm(mergedFocalTiles)
#         gc()
#         return(raster::raster(mergedTilesName))
#       }
#     })
#     names(trends) <- birdSpecies
#     return(trends)  
#   }