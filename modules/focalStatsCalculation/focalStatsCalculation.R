defineModule(sim, list(
  name = "focalStatsCalculation",
  description = paste0("This module is a revised and combined version of the prepTiles and",
                       " focalCalculation modules. It is flexible to take in a list of any rasters",
                       " and was developed once we realized that the original data had changed", 
                       "It will consider that the raster to calculate the focal stats has the ", 
                       "'years for the calculation' depicted as the raster's values, and that ", 
                       "those years represent the year that given pixel changed from 0 to 1", 
                       "This assumption is necessary and is different from the previous assumptions",
                       "where the rasters were indeed binary and the product was composed of 3 ",
                       "separated rasters that had the same information"),
  keywords = c("focal", "big data", "rasters"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", 
                                role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", comment = NULL)), 
                      class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.1", focalStatsCalculation = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "focalStatsCalculation.Rmd")),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter("nx", "numeric", 2, 1, NA, "the number of tiles to split raster into, along horizontal axis"),
    defineParameter("ny", "numeric", 2, 1, NA, "the number of tiles to split raster into, along vertical axis"),
    defineParameter("rType", "character", "INT1U", NA, NA, "pixel data type for splitRaster"),
    defineParameter("buffer", "numeric", 3, 0, NA, paste0("the number of cells to buffer tiles ",
                                                          "during splitRaster. Measured in cells, not distance")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, paste0("Should this entire module be ",
                                                                  "run with caching activated? ",
                                                                  "This is generally intended for ",
                                                                  "data-type modules, where ",
                                                                  "stochasticity and time are ",
                                                                  "not relevant")),
    defineParameter("useParallel", "character", NULL, NA, NA, 
                    "Should we parallelize tile processing?"),
    defineParameter("focalDistance", "numeric", 100, NA, NA, 
                    paste0("The distance at which to compute focal statistics, in units of", 
                           " the input rastesr CRS. This will be used to ", 
                           "create a matrix with circular weights summing to 1)")),
    defineParameter("cores", "numeric", NULL, NA, NA, paste0("How many nodes/cores to use when",
                                                             " parallelizing?", 
                                                             "If NULL and useParallel, calculates",
                                                             "an optimal"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "disturbanceRaster", objectClass = "RasterLayer", 
                 desc = paste0("Raster where pixels are representing the year of the disturbance.",
                               "Currently, we are using Hermosilla et al. 2019, available on",
                               paste0(
                                 "https://opendata.nfis.org/downloads/",
                                 "forest_change/CA_forest_harvest_mask",
                                 "_year_1985_2015.zip"))),
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer", 
                 desc = paste0("Raster that has only 1's inside the study area.",
                               "Basically the study area shapefile rasterized.",
                               "It is needed for fixing the disturbance layers if these only have ",
                               "values when the pixel has been disturbed, and NA's otherwise")),
    expectsInput(objectName = "studyArea", objectClass = "", 
                 desc = paste0("Shapefile of the study area.",
                               "It is needed only if the disturbance raster is not provided.",
                               "Used in .inputObjects but not in the simuation"))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "focalYearList", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one for a year of ",
                                "focal disturbances")),
    createsOutput(objectName = "splittedRTM", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one is a tile ")),
    createsOutput(objectName = "splittedDisturbance", objectClass = "list", 
                  desc = paste0("This is a list of rasters, each one is a tile ",
                                "At this time, it hasn't yet been processed"))
  )
))

doEvent.focalStatsCalculation = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- scheduleEvent(sim, start(sim), "focalStatsCalculation", "tile")
      sim <- scheduleEvent(sim, start(sim), "focalStatsCalculation", "calculatingFocal")
    },
    tile = {
      
      browser() # Work on the functions below
      sim$splittedRTM <- Cache(splitRaster, r = sim$Raster3,
                           nx = params(sim)$prepTiles$nx, 
                           ny = params(sim)$prepTiles$ny, 
                           buffer = params(sim)$prepTiles$buffer,  # Splitting disturbanceYear Raster, write to disk,
                           rType = params(sim)$prepTiles$rType,
                           path = file.path(cachePath(sim), "Raster3"),
                           cacheId = paste0("splitRaster3_x", params(sim)$prepTiles$nx, "y", 
                                            params(sim)$prepTiles$ny))
      sim$splittedDisturbance <- Cache(splitRaster, r = sim$Raster3,
                               nx = params(sim)$prepTiles$nx, 
                               ny = params(sim)$prepTiles$ny, 
                               buffer = params(sim)$prepTiles$buffer,  # Splitting disturbanceYear Raster, write to disk,
                               rType = params(sim)$prepTiles$rType,
                               path = file.path(cachePath(sim), "Raster3"),
                               cacheId = paste0("splitRaster3_x", params(sim)$prepTiles$nx, "y", 
                                                params(sim)$prepTiles$ny))
      
      
      
    },
    calculatingFocal = {
      # REVISE THE STRUCTURE OF FUNS BELOW... A LOT WAS DONE BECAUSE OF THE DIFFERENT 3 RASTERS 
      # WHICH WE DON'T HAVE ANYMORE...
      # sim$focalYearList[[paste0("Year", time(sim))]] <- Cache(applyFocalToTiles,
      #                                                         listTilePaths = sim$rastersList,
      #                                                         pathData = dataPath(sim),
      #                                                         pathCache = cachePath(sim),
      #                                                         forestClass = P(sim)$forestClass,
      #                                                         focalDistance = P(sim)$focalDistance,
      #                                                         disturbanceClass = P(sim)$disturbanceClass,
      #                                                         recoverTime = P(sim)$recoverTime,
      #                                                         resampledRes = P(sim)$resampledRes,
      #                                                         useParallel = P(sim)$useParallel,
      #                                                         nNodes = P(sim)$nNodes,
      #                                                         currentYear = time(sim), 
      #                                                         cacheId = paste0("focalToTiles", max(P(sim)$focalDistance),
      #                                                                          "m", time(sim)))

    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("studyArea", sim)){
    studyArea <- reproducible::Cache(usefulFuns::defineStudyArea,
                                     testArea = TRUE)
    
    sim$studyArea <- Cache(reproducible::postProcess, studyArea, 
                           destinationPath = SpaDES.core::Paths$inputPath,
                           filename2 = "OntarioStudyArea",
                           userTags = c("objectName:studyArea",
                                        cacheTags,
                                        "goal:sA"),
                           omitArgs = c("destinationPath"))
    
  }
  
  if (!suppliedElsewhere("disturbanceRaster", sim)){
    sim$disturbanceRaster <- reproducible::Cache(reproducible::prepInputs,
                                             url = paste0(
                                               "https://opendata.nfis.org/downloads/",
                                               "forest_change/CA_forest_harvest_mask",
                                               "_year_1985_2015.zip"),
                                             targetFile = "CA_harvest_year_1985_2015.tif",
                                             destinationPath = SpaDES.core::Paths$inputPath,
                                             filename2 = "disturbanceRasterProcessed",
                                             studyArea = sim$studyArea,
                                             userTags = c("objectName:disturbanceRaster",
                                                          cacheTags,
                                                          "outFun:Cache", "goal:prepDisturbanceRas"),
                                             omitArgs = c("overwrite", "destinationPath"))
  }
  
  if (!suppliedElsewhere("rasterToMatch", sim)){
    studyAreaSF <- sf::st_as_sf(sim$studyArea)
    studyAreaSF$RTM <- 1
    sim$rasterToMatch <- fasterize::fasterize(sf = studyAreaSF, 
                                          raster = sim$disturbanceRaster, 
                                          field = "RTM")
  }
  return(invisible(sim))
}
