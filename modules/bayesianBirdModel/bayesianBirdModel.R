
defineModule(sim, list(
  name = "bayesianBirdModel",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9011", bayesianBirdModel = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "bayesianBirdModel.Rmd"),
  reqdPkgs = list("googledrive", "data.table", "raster", "stats", "gstat", "LandR", "stringr", "nimble", "tati-micheletti/usefun@development"),
  parameters = rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("testArea", "logical", FALSE, NA, NA, "Should use study area?"),
    defineParameter("modelType", "numeric", 1, NA, NA, "Should use study area?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "birdDensityRasters", objectClass = "RasterStack", 
                 desc = "Raster stack of expected density of all birdSpecies", sourceURL = NA),
    expectsInput(objectName = "focalRasters", objectClass = "RasterStack", 
                 desc = "Raster stack of all disturbances 100 and 500m for all years", sourceURL = NA),
    expectsInput(objectName = "dataName", objectClass = "character", 
                 desc = "File name of used dataset", sourceURL = NA),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", 
                 desc = "Random polygon in Ontario for when testArea = TRUE", sourceURL = NA),
    expectsInput(objectName = "birdSpecies", objectClass = "character",
                 desc = "list of bird species to run the models for (same as Suarez 2019 et al.)", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "data", objectClass = "data.table", desc = "Bird dataset: Minidataset_master29JAN19.csv"),
    createsOutput(objectName = "fixedDT", objectClass = "list", desc = "Data.table of fixed values (model values) for the bayesian DT"),
    createsOutput(objectName = "yearDT", objectClass = "list", desc = "Data.table of yearly values (for prediction values) for the bayesian DT"),
    createsOutput(objectName = "hierarchicalModel", objectClass = "list", desc = "List of models for each bird species")
  )
))

doEvent.bayesianBirdModel = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      if (any(nchar(start(sim)) < 4, nchar(end(sim)) < 4)) # Sanity check on years
        stop("This module deals with explictit years (1985 - 2011). Please provide the time in YYYY format.")
      
      sim$data <- prepInputs(url = "https://drive.google.com/open?id=1KoL6QzKqCiBUZ8i6O-llCiit0G2APWLI", 
                             fun = "base::readRDS", 
                             targetFile = "data.rds",
                             destinationPath = dataPath(sim))
      
      # schedule future event(s)
      if (start(sim) == time(sim))
        sim <- scheduleEvent(sim, time(sim), "bayesianBirdModel", "model")
      # sim <- scheduleEvent(sim, time(sim), "bayesianBirdModel", "prediction")
    },
    model = {
      message(paste0("Building the statistical data frame...")) # Takes about 40Gb per species. Paralellize!
      sim$fixedDT <- dataframeBuilding(birdData = sim$data, 
                                       birdSpecies = sim$birdSpecies)
      sim$hierarchicalModel <- lapply(X = seq_along(sim$birdSpecies),
                                               FUN = function(index){
                                                 predictHierarchicalModel(
                                                   bird = index,
                                                   birdList = sim$birdSpecies,
                                                   currentYearBirdData = sim$fixedDT[[index]],
                                                   currentTime = time(sim),
                                                   pathData = dataPath(sim),
                                                   modelType = P(sim)$modelType)
                                               })
      names(sim$hierarchicalModel) <- sim$birdSpecies
      saveRDS(sim$hierarchicalModel$BBWA, file.path(Paths$outputPath, 
                                                    paste0("model", 
                                                           P(sim)$modelType,
                                                           ".rds")))
      drive_upload(file.path(Paths$outputPath, paste0("model", P(sim)$modelType, ".rds")),
                   as_id("1BAbKC-D4cGgBF2Sl9r_zkFcD2XOjh4wt"))
    },
    prediction = {
      message(paste0("Predicting from the statistical data frame...")) # Takes about 40Gb per species. Paralellize!
      browser() # Predict and Rebuild and the rasters
      sim$yearDT <- lapply(X = seq_along(sim$birdSpecies), # future_lapply
                           FUN = function(index){
                             buildYearlyDT(
                               currentTime = time(sim),
                               pathData = dataPath(sim),
                               rP = sim$rP,
                               fixedDT = sim$fixedDT[[index]],
                               focalRasters = sim$focalRasters,
                               birdDensityRasters = sim$birdDensityRasters[[index]])
                           })
      names(sim$yearDT) <- sim$birdSpecies
    },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  if(!suppliedElsewhere("dataName", sim)){
    sim$dataName <- "Minidataset_master29JAN19.csv"
  }
  if (!suppliedElsewhere("focalYearList", sim)){
    allFiles <- list.files(dataPath(sim), full.names = TRUE)
    focalRastersPaths <- grepMulti(patterns = c("mergedFocal", ".tif"), x = allFiles)
    if (length(focalRastersPaths) == 0){
      preProcess(url = "https://drive.google.com/open?id=1OtSXUGspv8UzPahmBOMDthkFm88HY3OK",
                 archive = "mergedFocalYYYY-DDDResXXXm.zip",
                 destinationPath = dataPath(sim))
      allFiles <- list.files(dataPath(sim), full.names = TRUE)
      focalRastersPaths <- grepMulti(patterns = c("mergedFocal", ".tif"), x = allFiles)
    }
    
    sim$focalRasters <- raster::stack(lapply(focalRastersPaths, FUN = raster))
    
  } else {
    sim$focalRasters <- raster::stack(sim$focalYearList)
    warning("Is the focalRasters stack really a stack that has both 100 and 500m focal calculations?
            if not, please DO NOT provide the file name (i.e. do not add the module focalCalculations). 
            This will be fixed in future versions")
  }
  
  if (!suppliedElsewhere("rP", sim)){
    # Default = Canadian boreal
    sim$rp <- defineStudyArea(testArea = TRUE, 
                              specificTestArea = "boreal", 
                              mapSubset = "Canada")
  }
  
  if (!suppliedElsewhere("birdSpecies", sim)){
    sim$birdSpecies = c("BBWA", "BLPW", "BOCH", "BRCR",
                        "BTNW", "CAWA", "CMWA","CONW",
                        "OVEN", "PISI", "RBNU", "SWTH",
                        "TEWA", "WETA", "YRWA")
  }
  if (!suppliedElsewhere("birdDensityRasters", sim)){
    sim$birdDensityRasters <- raster::stack(lapply(X = sim$birdSpecies, FUN = function(sp){
      fileToLoad <- grepMulti(x = list.files(path = dataPath(sim), full.names = TRUE), 
                              patterns = c("density", sp, ".tif"))
      if (length(fileToLoad) == 0){
        preProcess(url = "https://drive.google.com/open?id=1Htxc5Wv-30B0nJGpIyKa711x8cfGFrac", 
                   destinationPath = dataPath(sim))
        fileToLoad <- grepMulti(x = list.files(path = dataPath(sim), full.names = TRUE), 
                                patterns = c("density", sp, ".tif"))
      }
      ras <- raster::raster(fileToLoad)
    })
    )
    names(sim$birdDensityRasters) <- sim$birdSpecies
  }
  
  return(invisible(sim))
}
