defineModule(sim, list(
  name = "habitatDensityMasking",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = c(person(given = "Tati", family = "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.2", habitatDensityMasking = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "habitatDensityMasking.Rmd"),
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
    expectsInput(objectName = "pathData", objectClass = "list",
                 desc = paste0("destination path to the new trend rasters. If not supplied, will be in tempdir()")),    
    expectsInput(objectName = "trends", objectClass = "list",
                 desc = paste0("List of species, with significant slope raster. ",
                               "The slope has was masked by significancy at alpha = 0.05")),
    expectsInput(objectName = "birdsRangeList", objectClass = "list",
                 desc = paste0("List of raster url for HR"))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "trendsWithinRange", objectClass = "list", desc = "trends masked to species habitats")
  )
))

doEvent.habitatDensityMasking = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
 
      sim <- scheduleEvent(sim, end(sim), "habitatDensityMasking", "maskToHR", eventPriority = .last())
    },
    maskToHR = {
      sim$trendsWithinRange <- maskingHR(trendRasters = sim$trends,
                                 birdsRangeList = sim$birdsRangeList,
                                 pathData = sim$pathData)
      },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  if (!suppliedElsewhere("birdSpecies", sim)) {
    sim$birdSpecies <- c("BBWA", "BLPW", "BOCH", "BRCR",
                         "BTNW", "CAWA", "CMWA", "CONW", 
                         "OVEN", "PISI", "RBNU", "SWTH", 
                         "TEWA", "WETA", "YRWA")
    }
  if (!suppliedElsewhere("birdsRangeList", sim)) {
    sim$birdsRangeList <- createBirdsRangeRasters(birdSpecies = sim$birdSpecies)
  }
  return(invisible(sim))
}