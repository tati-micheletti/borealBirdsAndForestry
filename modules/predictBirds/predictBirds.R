defineModule(sim, list(
  name = "predictBirds",
  description = paste0("This module is intended to predict bird density", 
                       " given a model (i.e. coming from glmerBirdModels) and ",
                       "a raster with covariates for prediction (i.e. focalCalculation)"),
  keywords = c("boreal birds", "prediction", "disturbances"), # c("insert key words here"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.2", predictBirds = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "predictBirds.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    defineParameter("useParallel", "logical", FALSE, NA, NA, 
                    paste0("Use parallel? Especifically for this module, when using a 1TB 56cores BorealCloud",
                           "not using paralell was 43% faster than using it, even if passing all rasters in memory,",
                           " and not writing nor reading from disk")),
    defineParameter("focalDistance", "numeric", 100, NA, NA, 
                    paste0("The distance at which to compute focal statistics, in units of", 
                           " the input rastesr CRS. This will be used to ", 
                           "create a matrix with circular weights summing to 1)")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, 
                    paste0("Should this entire module be run with caching activated?",
                           " This is generally intended for data-type modules, where ",
                           "stochasticity and time are not relevant"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "focalYearList", objectClass = "list", 
                 desc = paste0("This list is structured in a way that the ",
                               "masked value passed is the first level of ",
                               "the list, which is composed of a full patched",
                               " raster with all merged tiles")),
    expectsInput(objectName = "birdSpecies", objectClass = "character", 
                 desc = "List of bird species to be modeled"),
    expectsInput(objectName = "models", objectClass = "list", 
                 desc = "list of boreal bird models"),
    expectsInput(objectName = "birdDensityRasters", objectClass = "list", 
                 desc = paste0("list of rasters with information",
                               " on species densities based on LCC and BCR/Prov")),
    expectsInput(objectName = "disturbancePredict", objectClass = "character", 
                 desc = paste0("Which type of disturbance will",
                               " be considered")),
    expectsInput(objectName = "predictModels", objectClass = "list",
                  desc = paste0("list of models for prediction. might be created or passed")),
    expectsInput(objectName = "nCores", objectClass = "character",
                 desc = paste0("Number of cores to use for parallel. Possible values: auto, numeric, NULL")),
    expectsInput(objectName = "predictModels", objectClass = "list",
                  desc = paste0("list of models for prediction. might be created or passed"))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "predictRas", objectClass = "list", 
                  desc = "List of years, which is a list of species with density rasters"),
    createsOutput(objectName = "predictModels", objectClass = "list",
                  desc = paste0("list of models for prediction. might be created or passed"))
  )
))

doEvent.predictBirds = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "predictBirds", "predictBirdsDensities")
    },
    predictBirdsDensities = {

      if (is.null(sim$predictModels)){ # Data sanity check
          stop("PredictModels was not found and default did not load test data. Revise code")
        }
        if (!identical(sim$birdSpecies, names(sim$predictModels)) | 
            !identical(sim$birdSpecies, names(sim$birdDensityRasters))){ # Data sanity check
          stop("birdSpecies, predictModels and densityRasters don't match the species' order. Revise code")
        }
      
      predictedName <- as.list(file.path(dataPath(sim), paste0("predicted", sim$birdSpecies, max(sim$focalDistance),
                                                               "mYear", time(sim), ".tif")))
      names(predictedName) <- sim$birdSpecies
      
        allPredictionsExist <- all(unlist(lapply(predictedName, FUN = function(yearSpPrediction){
          fileExists <- file.exists(yearSpPrediction)
          return(fileExists)
        })))
        
        if (allPredictionsExist){
          sim$predictRas[[paste0("Year", time(sim))]] <- lapply(X = sim$birdSpecies, FUN = function(bird){
            ras <- raster::raster(predictedName[[bird]])
          })
          names(sim$predictRas[[paste0("Year", time(sim))]]) <- sim$birdSpecies      
          
        } else {
          
          if (sim$nCores == "auto") {
            sim$nCores <- pemisc::optimalClusterNum(70000, maxNumClusters = length(birdSpecies))
          }
          if (all(.Platform$OS.type != "windows", isTRUE(P(sim)$useParallel))) {
            cl <- parallel::makeForkCluster(sim$nCores, outfile = file.path(dataPath(sim), "logParallelBirdPrediction")) # Tried, works, too slow
            # cl <- parallel::makePSOCKcluster(sim$nCores, outfile = file.path(dataPath(sim), "logParallelBirdPrediction")) # Tried, also works, also slow
            
            on.exit(try(parallel::stopCluster(cl), silent = TRUE))
          } else {
            cl <- NULL
          }
          
          sim$focalYearList[[paste0("Year", time(sim))]][] <- sim$focalYearList[[paste0("Year", time(sim))]][]
          sim$birdDensityRasters <- lapply(sim$birdDensityRasters, function(r){
            r <- raster::raster(r)
            r[] <- r[]
            return(r)
          })
          
          # Snapping layers to make sure it will work
          sim$birdDensityRasters <- lapply(X = sim$birdDensityRasters, FUN = function(lay){
            raster::extent(lay) <- raster::alignExtent(extent = raster::extent(lay),
                                                       object = sim$focalYearList[[paste0("Year", time(sim))]],
                                                       snap = "near")
            return(lay)
          })
          
          birdDensityVectors <- lapply(sim$birdDensityRasters, FUN = function(ras){
            vec <- raster::getValues(ras)
            return(vec)
          })
          disturbanceRasVector <- raster::getValues(sim$focalYearList[[paste0("Year", time(sim))]])
          
          if (var(c(sapply(birdDensityVectors, length), length(disturbanceRasVector))) == 0){ # Data sanity check
            if (!is.null(cl)){
              message(crayon::red(paste0("Paralellizing for:\n", paste(sim$birdSpecies, collapse = "\n"),
                                         "\nUsing ", sim$nCores, " cores \n",
                                         "\nMessages will be suppressed until done")))
              predictVec <- clusterApplyLB(seq_along(sim$birdSpecies),
                                           cl = cl, function(index) {
                                             corePrediction(bird = sim$birdSpecies[[index]],
                                                            model = sim$predictModels[[index]],
                                                            predictedName = predictedName[[index]],
                                                            birdDensityRas = birdDensityVectors[[index]],
                                                            pathData = dataPath(sim),
                                                            disturbanceRas = disturbanceRasVector,
                                                            currentTime = time(sim))
                                           })
            } else {
              predictVec <- lapply(seq_along(sim$birdSpecies),
                                   function(index) {
                                     corePrediction(bird = sim$birdSpecies[[index]], 
                                                    model = sim$predictModels[[index]], 
                                                    predictedName = predictedName[[index]],
                                                    birdDensityRas = birdDensityVectors[[index]],
                                                    pathData = dataPath(sim),
                                                    disturbanceRas = disturbanceRasVector,
                                                    currentTime = time(sim))
                                   })
            }
            
            # Reconvert vectors into rasters
            rm(disturbanceRasVector)
            rm(birdDensityVectors)
            invisible(gc())
            sim$predictRas[[paste0("Year", time(sim))]] <- lapply(predictVec, FUN = function(spVec){
              rasName <- paste0("prediction", attributes(spVec)[["prediction"]])
              birdRas <- raster(sim$birdDensityRasters[[1]]) # Using the first as a template. All should be the same.
              birdRas <- raster::setValues(x = birdRas, values = as.numeric(spVec))
              return(birdRas)
            })
            names(sim$predictRas[[paste0("Year", time(sim))]]) <- sim$birdSpecies      
            rm(predictVec)
            invisible(gc())
            
          } else {
            stop("There is a mismatch among the birdDensityRasters and/or the disturbance raster. Please revise the code.")
          }
          
          # SAVE THE PREDICTED RASTERS
          sim$predictRas[[paste0("Year", time(sim))]] <- lapply(names(predictedName), function(bird){
            writeRaster(sim$predictRas[[paste0("Year", time(sim))]][[bird]], 
                        filename = predictedName[[bird]], format = "GTiff")
            return(predictedName[[bird]])
          })
          names(sim$predictRas[[paste0("Year", time(sim))]]) <- sim$birdSpecies
          
          invisible(gc())
        }
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + 1, "predictBirds", "predictBirdsDensities")
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if (!suppliedElsewhere("nCores", sim)){
    sim$nCores <- "auto"
  }
  
  if (!is.null(unlist(sim@params,
                      use.names = FALSE)[grepl(pattern = "focalDistance", 
                                               x = names(unlist(sim@params)))])){
    sim$focalDistance <- max(as.numeric(unlist(sim@params, 
                                               use.names = FALSE)[grepl(pattern = "focalDistance", 
                                                                        x = names(unlist(sim@params)))]))
  }
  if (!suppliedElsewhere("focalYearList", sim)){
    message(paste0("No disturbance list of rasters found.", 
                   "Will try checking on focalCalculation folder and creating it... ", 
                   start(sim), ":", end(sim)))
    if (file.exists(paste0("modules/focalCalculation/data/mergedFocal1985-", 
                           max(sim$focalDistance), "Res250m.tif"))){
    sim$focalYearList <- list()
    sim$focalYearList <- lapply(X = start(sim):end(sim), FUN = function(yr){
      ras <- paste0("modules/focalCalculation/data/mergedFocal", 
                    yr, "-", max(sim$focalDistance), "Res250m.tif")
      doesIt <- file.exists(ras)
      if (doesIt) return(raster::raster(ras)) else return(NULL)
    })
    names(sim$focalYearList) <- paste0("Year", start(sim):end(sim))
    if (any(is.null(unlist(sim$focalYearList)))) message("At least one focal raster is null, this year will not be 
                                                         predicted for or will return error")
    } else {
      message(paste0("No disturbance list of rasters found.", 
                     "Using fake disturbance raster for years ", 
                     start(sim), ":", end(sim)))
      sim$focalYearList <- Cache(fakeFocalRasterYears, st = start(sim),
                                 ed = end(sim),
                                 res = c(250, 250),
                                 crsRas = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
      names(sim$focalYearList) <- paste0("Year", start(sim):end(sim))
    }
  }
  
  if (!suppliedElsewhere("birdDensityRasters", sim)){
    if (suppliedElsewhere("birdSpecies", sim)){
      stop(paste0("Bird list supplied, but density rasters not.",
                  " Please either provide either both objects, or ",
                  "none of these two."))
    } else {
    message(paste0("No bird density raster found.", 
                   "Using fake density raster for year BBWA and BOCH"))
        sim$birdSpecies <- c("BBWA", "BOCH")
    suppressWarnings(dir.create(file.path(dataPath(sim), "tmp")))
    sim$birdDensityRasters <- lapply(X = sim$birdSpecies, FUN = function(sp){
      tmpDir <- file.path(dataPath(sim), paste0("tmp/tempBirdDensityRas", sp, ".tif"))
      if (!file.exists(tmpDir)) {
        ras <- SpaDES.tools::gaussMap(raster::raster(xmn = -30^3, xmx = 30^3,
                                                     ymn = -30^3, ymx = 30^3,
                                                     resolution = c(250, 250)))
        raster::crs(ras) <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
        ys <- seq(from = 0.6, to = 0.9, by = 0.05)
        bff <- (max(ras[]) - min(ras[]))/length(ys)
        vecM <- numeric(length(ys))
        cnty <- 1
        mult <- 0
        for (i in seq(from = 1, to = length(ys)*3, by = 3)){
          vecM[i:(2+i)] <- c(bff*mult, bff*(mult+1), ys[cnty])
          mult <- mult + 1
          cnty <- cnty + 1
        }
        rclmat <- matrix(vecM, ncol=3, byrow=TRUE)
        ras <- raster::reclassify(x = ras, rcl = rclmat)
        raster::writeRaster(x = ras, filename = tmpDir, format = "GTiff", overwrite = TRUE)
      }
      sim$birdDensityRasters[[sp]] <- tmpDir
      return(sim$birdDensityRasters[[sp]])
    })
    names(sim$birdDensityRasters) <- sim$birdSpecies
    }
  }
  
  if (!suppliedElsewhere("predictModels", sim)){
    message("No models list found. Using fake generalized linear model for BBWA and BOCH")
    source(file = file.path(dataPath(sim), "fakeDataBirds.R"))
    sim$data <- data
    sim$predictModels <- lapply(X = sim$birdSpecies, FUN = function(sp){
      if (!max(P(sim)$focalDistance) %in% c(100, 500)) {
        stop("At the moment, fake models can only be generated with focalDistances of 100 or 500.")
      } else {
      suppressMessages(assign(sp, eval(parse(text = paste0("glm(AB_", sp, " ~ State_P_", max(P(sim)$focalDistance), 
                                                             " + LOG_BCR_", sp, ", ",
                                                             "offset = OF_", sp,
                                                             ", family = 'poisson', data = data)")))))
        return(get(sp))
      }
  })
    names(sim$predictModels) <- sim$birdSpecies
  }

  
  return(invisible(sim))
}
