
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "glmerBirdModels",
  description = "The present module is based on the work of Suárez-Esteban et al. (in prep). 
These models investigate the following questions:
1. Do cumulative disturbances at local scales have a consistent effect across Canada on old forest-associated songbirds? 
2. Do neighborhood disturbances influence the abundance of birds at local scales? 
3. What are the relative effects of permanent and transitional disturbance agents at both scales?. 
  For more information: Alberto Suárez-Esteban, Steve G. Cumming, Erin M. Bayne, Samantha J. Song, and Fiona K. A. Schmiegelow. 
The rise of industrial development, the fall of boreal songbirds: Industrial development threatens boreal songbirds across Canada",
  keywords = c("songbirds", "forestry", "Anthropogenic disturbance"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Alberto", "Suárez-Esteban", email = "suarezes@ualberta.ca", role = c("aut"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0.9000", glmerBirdModels = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "glmerBirdModels.Rmd"),
  reqdPkgs = list("data.table", "rgdal", "raster", "sf", "lme4"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("cropping", "logical", FALSE, NA, NA, "If the rasters should be cropped to a study area or not"),
    defineParameter("cropForModel", "logical", FALSE, NA, NA, "If the bird data should be cropped to a study area or not for fitting the model"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "birdData", objectClass = "data.table", desc = "Bird data assembled by the BAM (Boreal Avian Modelling Project)",
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "models", objectClass = "list", desc = "list of boreal bird models")
  )
))

## event types

doEvent.glmerBirdModels = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {

      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "glmerBirdModels", "birdModels")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "glmerBirdModels", "save")
    },
  
  birdModels = {
    
      sim$models <- birdModelsFunction(data = sim$birdData)
    
    },
    save = {
     
      sim <- saveFiles(sim)
      
    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

### Save events
Init <- function(sim) {
  
  sim$models <- list()
  
  return(invisible(sim))
}

.inputObjects = function(sim) {

if (sim$cropping==TRUE){
    sim$studyArea <- loadStudyArea(data = studyArea)
}  
    sim$birdData <- loadData(data = "Final_points_BEAD.csv")

  return(invisible(sim))
}
