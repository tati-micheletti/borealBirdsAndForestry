
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "splitModelPlot",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9012", splitModelPlot = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "splitModelPlot.Rmd"),
  reqdPkgs = list("raster", "rlist", "ggplot2", "ggfortify"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("focalDistance", "numeric", 100, NA, NA, 
                    "The distance at which to compute focal statistics, in units of the input rastesr CRS.
                    This will be used to create a matrix with circular weights summing to 1)"),
    defineParameter("disturbanceClass", "numeric", 2, 0, NA, 
                    "the class value(s) corresponding to input disturbanceType for which to compute focal statistics"),
    defineParameter("nx", "numeric", 2, 1, NA, "the number of tiles to split raster into, along horizontal axis"),
    defineParameter("ny", "numeric", 2, 1, NA, "the number of tiles to split raster into, along vertical axis"),
    defineParameter("rType", "character", "FLT4S", NA, NA, "pixel data type for splitRaster"),
    defineParameter("buffer", "numeric", 3, 0, NA, "the number of cells to buffer tiles during splitRaster. Measured in cells, not distance"),
    defineParameter("forestClass", "numeric", 1:6, NA, NA, "Relevant forest classes in land cover map"),
    defineParameter(".useCache", "logical", TRUE, NA, NA,"Should this entire module be run with caching activated?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "disturbanceType", objectClass = "RasterLayer", 
                 desc = "disturbanceType prouduct", sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Type.zip"),
    expectsInput(objectName = "disturbanceYear", objectClass = "RasterLayer", 
                 desc = "filepath to a raster layer representing year of disturbance occurence", 
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Year.zip"),
    expectsInput(objectName = "landCover", objectClass = "RasterLayer", desc = "Landcover classes. The default is LCC2005", 
                 sourceURL = "http://www.cec.org/sites/default/files/Atlas/Files/2010nalcms30m/can_landcover_2010_30m.rar"),
    expectsInput(objectName = "inputSpecies", objectClass = "list", 
                 desc = "a list of bird species", sourceURL = NA),
    expectsInput(objectName = "inputModels", objectClass = "list", 
                 desc = "a list of models corresponding to bird species", sourceURL = NA),
    expectsInput(objectName = "inputAbundances", objectClass = "list",
    desc = "a list of rasters represnting abundance and corresponding to species")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    # createsOutput(objectName = "abundanceTS", objectClass = "list", desc = "a list of time series"),
    createsOutput(objectName = "populationTrends", objectClass = "list", desc = "a list predicted trends in species, including rasters representing trend in abundance over study period, a list of time series plots and the time series themselves")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.splitModelPlot = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)

    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
    
    a <- Map(sim$inputSpecies, sim$inputModels, sim$inputAbundances, f = groupSplitRaster, 
             MoreArgs = list(sim = sim))
    names(a) <- inputSpecies
    sim$populationTrends <- a
    
    return(invisible(sim))
}



.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # SpaDES.tools::downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
