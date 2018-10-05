defineModule(sim, list(
  name = "predictBirds",
  description = paste0("This module is intended to predict bird density", 
                       " given a model (i.e. coming from glmerBirdModels) and ",
                       "a raster with covariates for prediction (i.e. focalCalculation)"),
  keywords = c("boreal birds", "prediction", "disturbances"), # c("insert key words here"),
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.2", predictBirds = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "predictBirds.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "focalYearList", objectClass = "list", 
                  desc = paste0("This list is structured in a way that the ",
                                "masked value passed is the first level of ",
                                "the list, which is composed of a full patched",
                                " raster with all merged tiles")),
    expectsInput(objectName = "counter", objectClass = "numeric",
                  desc = paste0("Counter of the number of tiles",
                                " used for saving the focal list")),
    expectsInput(objectName = "birdSpecies", objectClass = "character", 
                 desc = "List of bird species to be modeled"),
    expectsInput(objectName = "models", objectClass = "list", 
                 desc = "list of boreal bird models"),
    expectsInput(objectName = "birdDensityRasters", objectClass = "list", 
                 desc = paste0("list of rasters with information",
                               " on species densities based on LCC and BCR/Prov"))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.predictBirds = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # Check if the rasters align (focalYearList[[x]] and birdDensityRasters[[x]]) [ FIX ]
     
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "predictBirds", "plot")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "predictBirds", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  return(invisible(sim))
}
