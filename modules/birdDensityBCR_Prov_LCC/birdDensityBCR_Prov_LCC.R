
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "birdDensityBCR_Prov_LCC",
  description = paste0("This module accesses bird density estimates for undisturbed",
                       "areas based on lancover class, BCR (Bird Conservation Regions)",
                       "and ecoprovince (Boreal Avian Modelling Product:	BAM.Density1.03-current)"),
  keywords = c("boreal birds","density","geographic strata", "BCR-Prov/Terr","LCC","habitat class"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0.9000", birdDensityBCR_Prov_LCC = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "birdDensityBCR_Prov_LCC.Rmd"),
  reqdPkgs = list("data.table", "raster", "plyr", "crayon"),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("testArea", "logical", FALSE, NA, NA, "Should use study area?"),
    defineParameter("avoidAlbertosData", "logical", TRUE, NA, NA, "Should we use the most updated version of the BAM densities? Then TRUE"),
    defineParameter("extractFrom4kRasters", "logical", FALSE, NA, NA, 
                    paste0("Should the analysis extract data from the 4k bird density rasters", 
                           "publicly available? If FALSE, needs access to table providing", 
                           "density per species per province per BCR per landcover class")),
    defineParameter("testArea", "logical", FALSE, NA, NA, "Should use study area?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "birdsRangeList", objectClass = "list", desc = "List of rasters of BAM boreal birds' ranges (more accurate than BirdLifeInternational)", sourceURL = NA),
    expectsInput(objectName = "specificTestArea", objectClass = "character", desc = "Specific test area to crop to: 'boreal', or a province english name", sourceURL = NA),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", desc = "Random polygon in Ontario for when testArea = TRUE", sourceURL = NA),
    expectsInput(objectName = "densityEstimatesURL", objectClass = "character", desc = "density values calculated by BAM - Habitat_Association_by_jurisdiction(bamddb_Apr19-2012).csv", sourceURL = "https://drive.google.com/open?id=1Gd9VsKIXX_384UxQsJnwzsSZFuyQyscw"),
    expectsInput(objectName = "birdSpecies", objectClass = "character", desc = "list of species to download density information based on LCC and BCR/Prov", sourceURL = NA),
    expectsInput(objectName = "densityEstimatesFileName", objectClass = "character", desc = "density file name", sourceURL = NA),
    expectsInput(objectName = "mapSubset", objectClass = "character", desc = "Subset of the map", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "birdDensityRasters", objectClass = "list", desc = paste0("list of rasters with information", 
                                                                                         " on species densities based on LCC and BCR/Prov")),
    createsOutput(objectName = "birdDensityDS", objectClass = "data.table", desc = paste0("list of data.table with information on species",
                                                                                          " densities based on LCC and BCR/Prov"))
  )
))

## event types

doEvent.birdDensityBCR_Prov_LCC = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "birdDensityBCR_Prov_LCC", "fetchData")
    },
    
    fetchData = {
      
      sim$birdDensityRasters <- fetchData(pathData = dataPath(sim), 
                                          birdSp = sim$birdSpecies, 
                                          birdsRangeList = sim$birdsRangeList,
                                          studyArea = sim$rP, 
                                          extractFrom4kRasters = P(sim)$extractFrom4kRasters,
                                          densityEstimatesURL = sim$densityEstimatesURL,
                                          densityEstimatesFileName = sim$densityEstimatesFileName,
                                          avoidAlbertosData = P(sim)$avoidAlbertosData,
                                          simEnv = envir(sim))
      
      sim$birdDensityDS <- birdDensityDS # Being created in the previous function and assigned to sim. Here just for a matter of transparency.
      
    },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects = function(sim) {
  
  if (!suppliedElsewhere('densityEstimatesURL', sim)) {
    sim$densityEstimatesURL <- extractURL("densityEstimatesURL")
  }
  
  if (!suppliedElsewhere('densityEstimatesFileName', sim)) {
    sim$densityEstimatesFileName <- "Habitat_Association_by_jurisdiction(bamddb_Apr19-2012).csv"
  }
  
  if (!suppliedElsewhere('birdSpecies', sim)) {
    sim$birdSpecies <- c("BBWA", "BLPW", "BOCH", "BRCR", 
                         "BTNW", "CAWA", "CMWA", "CONW", 
                         "OVEN", "PISI", "RBNU", "SWTH", 
                         "TEWA", "WETA", "YRWA")
  }
  
  if (!suppliedElsewhere("rP", sim)) {
    sim$rP <- defineStudyArea(testArea = P(sim)$testArea, 
                              specificTestArea = sim$specificTestArea, 
                              mapSubset = sim$mapSubset, 
                              destinationFolder = dataPath(sim))
  }
  
  if (!suppliedElsewhere("birdsRangeList", sim)) {
    sim$birdsRangeList <- createBirdsRangeRasters(birdSpecies = sim$birdSpecies)
  }
  return(invisible(sim))
}

