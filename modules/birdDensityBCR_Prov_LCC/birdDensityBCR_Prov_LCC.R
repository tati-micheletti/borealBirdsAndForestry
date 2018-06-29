
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "birdDensityBCR_Prov_LCC",
  description = paste0("This module accesses bird density estimates for undisturbed",
                       "areas based on lancover class, BCR (Bird Conservation Regions)",
                       "and ecoprovince (Boreal Avian Modelling Product:	BAM.Density1.03-current)",
                       "citation: Stralberg, D., S. M. Matsuoka, A. Hamann, E. M. Bayne, P. Sólymos,", 
                       "F. K. A. Schmiegelow, et al. 2015. Projecting boreal bird responses to climate", 
                       "change: the signal exceeds the noise. Ecological Applications 25:52–69.", 
                       "http://dx.doi.org/10.1890/13-2289.1"),
  keywords = c("boreal birds","density","geographic strata", "BCR-Prov/Terr","LCC","habitat class"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
            person("Diana", "Stralberg", email = "stralber@ualberta.ca", role = c("aut"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0.9000", birdDensityBCR_Prov_LCC = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "birdDensityBCR_Prov_LCC.Rmd"),
  reqdPkgs = list("data.table", "raster"),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("testArea", "logical", FALSE, NA, NA, "Should use study area?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "birdSpecies", objectClass = "character", desc = "list of species to download density information based on LCC and BCR/Prov", sourceURL = NA),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", desc = "Random polygon in Ontario for when testArea = TRUE", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = c("birdDensityRasters","birdDensityTables"), 
                  objectClass = c("list","list"),
                  desc = c("list of rasters with information on species densities based on LCC and BCR/Prov",
                           "list of data.table with information on species densities based on LCC and BCR/Prov"))
  )
))

## event types

doEvent.birdDensityBCR_Prov_LCC = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "birdDensityBCR_Prov_LCC", "fetchData")
      sim <- scheduleEvent(sim, start(sim), "birdDensityBCR_Prov_LCC", "extractData")
      },

    fetchData = {

      sim$birdDensityRasters <- fetchData(pathData = dataPath(sim), birdSp = sim$birdSpecies, studyArea = sim$rP)
      
    },
    
    extractData = {
      
      # sim$birdDensityTables <- extractData(datasetRaster = sim$birdDensityRasters, 
      #                                      typeData = "list", 
      #                                      list = sim$birdSpecies) # Revise function and name
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects = function(sim) {
  
  if (!suppliedElsewhere('birdSpecies', sim)) {
    sim$birdSpecies <- c("BBWA", "BLPW", "BOCH", "BRCR", 
                         "BTNW", "CAWA", "CMWA", "CONW", 
                         "OVEN", "PISI", "RBNU", "SWTH", 
                         "TEWA", "WETA", "YRWA")
  }
  
  if(!is.null(P(sim)$testArea) & P(sim)$testArea == TRUE){
    sim$polyMatrix <- matrix(c(-93.028935, 50.271979), ncol = 2)
    sim$areaSize <- 5000000
    set.seed(1234)
    sim$rP <- randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon    
    message("Test area is TRUE. Cropping and masking to an area in south Ontario.")
  } else {
    sim$rP <- NULL
  }
  
  return(invisible(sim))
}

