defineModule(sim, list(
  name = "focalCalculation",
  description = paste0("This module reorganizes tiles from another module", 
                       "(matches the exact tile location from all different lists),",
                       "applies a binary function to certain rasters of all tiles ", 
                       "of a list of a list of rasters, applies a focal function", 
                       " to these tiles and resamples them, merge, and returns", 
                       "a list of resample tiles masked to a given value", 
                       " (in this case, per year)."),
  keywords =  c("focal", "big data", "raster"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.2", focalCalculation = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "focalCalculation.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    defineParameter("forestClass", "numeric", 1:6, NA, NA, 
                    "Relevant forest classes in land cover map"),
    defineParameter("disturbanceClass", "numeric", 2, 0, NA, 
                    paste0("the class value(s) corresponding to input", 
                           " disturbanceType for which to compute focal statistics")),
    defineParameter("focalDistance", "numeric", 100, NA, NA, 
                    paste0("The distance at which to compute focal statistics, in units of", 
                           " the input rastesr CRS. This will be used to ", 
                           "create a matrix with circular weights summing to 1)")),
    defineParameter("resampledRes", "numeric", 250, NA, NA, 
                    "Resolution to which the final focal raster should be resample to"),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should this entire module be run with caching activated?"),
    defineParameter("useParallel", "character", NULL, NA, NA, 
                    "Should we parallelize tile processing?") # Check if I am actually gonna use it
    ),
  inputObjects = bind_rows(
    expectsInput(objectName = "listTilePaths", objectClass = "character", 
                 desc = paste0("Character listing the raster paths to the tiles from",
                               " Raster3"))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "focalYearList", objectClass = "list", 
                  desc = paste0("This list is structured in a way that the ",
                                "masked value passed is the first level of ",
                                "the list, which is composed of a full patched",
                                " raster with all merged tiles")),
    createsOutput(objectName = "counter", objectClass = "numeric",
                  desc = paste0("Counter of the number of tiles",
                                " used for saving the focal list"))
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.focalCalculation = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      sim$focalYearList <- list()
      
      if (is.null(sim$listTilePaths)) {
        stop(paste0("No list of tile paths was provided ",
                    "and the dummy default has not loaded. 
                      Check module code."))
      }
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "focalCalculation", "focalOperations")
      
    },
    
    focalOperations = {
      browser()
      sim$focalYearList[[paste0("Year", time(sim))]] <- Cache(applyFocalToTiles, #useParallel = P(sim)$useParallel, 
                                                                # We will use parallel only if when all is in memory, 
                                                                # it leaves space in memory for dealing with more than 1 at a time
                                                                listTilePaths = sim$listTilePaths,
                                                                pathData = dataPath(sim),
                                                                forestClass = P(sim)$forestClass,
                                                                focalDistance = P(sim)$focalDistance,
                                                                disturbanceClass = P(sim)$disturbanceClass,
                                                                recoverTime = P(sim)$recoverTime,
                                                                resampledRes = P(sim)$resampledRes,
                                                                currentYear = time(sim))
      
      sim <- scheduleEvent(sim, time(sim) + 1, "focalCalculation", "focalOperations")
      
    },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  if (!suppliedElsewhere("listTilePaths", sim)) {
    sim$listTilePaths <- Cache(createRandomRasterList, rastersPerList = 5, numberOfLists = 3)
    message(crayon::yellow(paste0("List of tile paths not found (no other module is creating it). ",
                                  "Using a dummy list of rasters"))) # [ FIX ] Need to make really tiled rasters
  }
  
  return(invisible(sim))
}

