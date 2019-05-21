
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
  reqdPkgs = list("googledrive", "data.table", "raster", "stats", "gstat", "LandR", "stringr"),
  parameters = rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("testArea", "logical", FALSE, NA, NA, "Should use study area?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "focalRasters", objectClass = "RasterStack", desc = "Raster stack of all disturbances 100 and 500m for all years", sourceURL = NA),
    expectsInput(objectName = "dataName", objectClass = "character", desc = "File name of used dataset", sourceURL = NA),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster", sourceURL = "ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif"),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", desc = "Random polygon in Ontario for when testArea = TRUE", sourceURL = NA),
    expectsInput(objectName = "birdSpecies", objectClass = "character", desc = "list of bird species to run the models for (same as Suarez 2019 et al.)", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "data", objectClass = "data.table", desc = "Bird dataset: Minidataset_master29JAN19.csv"),
    createsOutput(objectName = "df", objectClass = "list", desc = "Dataframe used for the bayesian model"),
    createsOutput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster (LCC05)")
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
      
      sim$ageMap <- loadAndProcessAgeMap(dataPath = dataPath(sim), 
                                         projection = "+proj=longlat +datum=WGS84",
                                         filename2 = "ageMap2004")
      
      
      
    
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "bayesianBirdModel", "model")
    },
    model = {
      
      sim$df <- dataframeBuilding(birdData = sim$data, 
                                  birdSpecies = sim$birdSpecies,
                                  ageMap = sim$ageMap,
                                  currentTime = time(sim),
                                  pathData = dataPath(sim),
                                  rP = sim$rP)
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
    sim$focalRasters <- raster::stack(focalYearList)
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
  
  return(invisible(sim))
  }
