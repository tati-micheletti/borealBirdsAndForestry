
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
    expectsInput(objectName = "birdDensityRasters", objectClass = "RasterStack", desc = "Raster stack of expected density of all birdSpecies", sourceURL = NA),
    expectsInput(objectName = "focalRasters", objectClass = "RasterStack", desc = "Raster stack of all disturbances 100 and 500m for all years", sourceURL = NA),
    expectsInput(objectName = "dataName", objectClass = "character", desc = "File name of used dataset", sourceURL = NA),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster", sourceURL = "ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif"),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", desc = "Random polygon in Ontario for when testArea = TRUE", sourceURL = NA),
    expectsInput(objectName = "birdSpecies", objectClass = "character", desc = "list of bird species to run the models for (same as Suarez 2019 et al.)", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "data", objectClass = "data.table", desc = "Bird dataset: Minidataset_master29JAN19.csv"),
    createsOutput(objectName = "fixedDT", objectClass = "list", desc = "Data.table of fixed values (model values) for the bayesian DT"),
    createsOutput(objectName = "yearDT", objectClass = "list", desc = "Data.table of yearly values (for prediction values) for the bayesian DT"),
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
      
      sim$fixedDT <- dataframeBuilding(birdData = sim$data, 
                                  birdSpecies = sim$birdSpecies,
                                  ageMap = sim$ageMap)
      
      browser() # Need to build the next function
      # Don't forget:
      # Need to add the expected age at the time currentTime (similar to what I did in dataframeBuilding)
      # Need to crop, mask and extract the values from the rasters to a DT:
      #     ageMap (YES, this too!)
      #     focalRasters: need to grep which ones are currentYear
      #     birdDensityRasters: don't forget to log these!
      # Think about cummulative disturbance: should this be per year? (i.e. dist1986 = dist1986-dist1985) THINK THIS WOULD COMPLICATE TOO MUCH!
      # I would just keep it as is (cumm dist) and just age the forest 
      sim$yearDT <- buildYearlyDT(currentTime = time(sim),
                                  pathData = dataPath(sim),
                                  rP = sim$rP,
                                  ageMap = sim$ageMap,
                                  fixedDT = sim$fixedDT,
                                  focalRasters = sim$focalRasters,
                                  birdDensityRasters = sim$birdDensityRasters)


      # 5. Create the data.frame that I want to populate: - NEXT FUNCTION. 
      # This one should be cached just to be added every year to the one I really want to monitor
      # 
      # Abundance = NA (the one parameter to monitor!),
      # estimate = log of estimate based on LCC_BCR (I have this table somewhere!),
      # age = corrected growth 1985-2011 based off of 2004 layer (and maybe double check the disturbances?! See item 5.),
      # disturbance = values from the focal rasterstack, 
      # X = coordenadas X from "template raster" (one of the mergedFocal?) for all pixels to 
      # forecast (maybe would be smart to crop to only the pixels that are within Sp distribution for computation time?),
      # Y = same as x,
      # offsets = NA
      # 
      # 6. Put the DF in the model and figure out after that...  
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
  }
  
  return(invisible(sim))
  }
