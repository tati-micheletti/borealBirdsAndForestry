
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
  reqdPkgs = list("googledrive", "data.table", "raster"),
  parameters = rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "dataName", objectClass = "character", desc = "File name of used dataset", sourceURL = NA),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster", sourceURL = "ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif"),
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "birdData", objectClass = "data.table", desc = "Bird dataset"),
    createsOutput(objectName = "models", objectClass = "list", desc = "list of boreal bird models")
  )
))

doEvent.bayesianBirdModel = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
     
      # do stuff for this event
      sim$birdData <- datasetUploading(dataPath = dataPath(sim), data = sim$dataName)
      sim$ageMap <- loadAndProcessAgeMap(dataPath = dataPath, rasterToMatch = #"FIND_A_RASTER_TO_MATCH_THIS_BIRD_PROJECTIONS!")

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "bayesianBirdModel", "model")
      
    },
    model = {
      
      sim$models <- bayesModel(birdData = sim$birdData)
    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if(!suppliedElsewhere("dataName", sim)){
    sim$dataName <- "Final_points_BEAD_final.csv"    
  }
  
  return(invisible(sim))
}
