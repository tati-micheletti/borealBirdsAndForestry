
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
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "birdSpecies", 
                 objectClass = "character", 
                 desc = "list of species to download density information based on LCC and BCR/Prov",
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
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
  
      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "birdDensityBCR_Prov_LCC", "fetchData")
      sim <- scheduleEvent(sim, start(sim), "birdDensityBCR_Prov_LCC", "extractData") #CHGECK IF MY FUNCTION extractData is good for it
      },

    fetchData = {
      
      sim$birdDensityRasters <- fetchData(sim = sim, birdSp = sim$birdSpecies)
      
    },
    
    extractData = {
      
      sim$birdDensityTables <- extractData(datasetRaster = sim$birdDensityRasters, 
                                           typeData = "list", 
                                           list = sim$birdSpecies) # Revise function name
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}


Init <- function(sim) {


  return(invisible(sim))
}

.inputObjects = function(sim) {

  if (!('birdSpecies' %in% sim$.userSuppliedObjNames)) { #Benchmatk later comaring to is.null(sim$birdSpecies)
   sim$birdSpecies <- c("BBWA", "BLPW", "BOCH", "BRCR", 
                        "BTNW", "CAWA", "CMWA", "CONW", 
                        "OVEN", "PISI", "RBNU", "SWTH", 
                        "TEWA", "WETA", "YRWA")
  }

  return(invisible(sim))
}

