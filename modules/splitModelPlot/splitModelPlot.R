
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
  reqdPkgs = list("raster", "rlist", "ggplot2", "ggfortify", "trend"),
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
    defineParameter(".useCache", "logical", c("init", "fetchGIS"), NA, NA,"Should this entire module be run with caching activated?"),
    defineParameter("testArea", "logical", FALSE, NA, NA, "Should use study area?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "disturbanceType", objectClass = "RasterLayer", 
                 desc = "disturbanceType prouduct", 
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Type.zip"),
    expectsInput(objectName = "disturbanceYear", objectClass = "RasterLayer", 
                 desc = "filepath to a raster layer representing year of disturbance occurence", 
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Year.zip"),
    expectsInput(objectName = "landCover", objectClass = "RasterLayer", desc = "Landcover classes. The default is LCC2010 30m", 
                 sourceURL = "http://www.cec.org/sites/default/files/Atlas/Files/2010nalcms30m/can_landcover_2010_30m.rar"),
    
    expectsInput(objectName = "inputSpecies", objectClass = "list", 
                 desc = "a list of bird species", sourceURL = NA),
    expectsInput(objectName = "models", objectClass = "list", 
                 desc = "a list of models corresponding to bird species", sourceURL = NA),
    expectsInput(objectName = "birdDensityRasters", objectClass = "list",
    desc = "a list of rasters representing abundance and corresponding to species"),
    expectsInput(objectName = "models", objectClass = "list", 
                 desc = "a list of models corresponding to bird species", sourceURL = NA),
    expectsInput(objectName = "landCoverDS", objectClass = "character",
                 desc = "name of land cover raster for identifying pixels that are 'forest'"),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", desc = "Random polygon in Ontario for when testArea = TRUE", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "populationTrends", objectClass = "list", 
                  desc = paste0("a list predicted trends in species, including rasters",
                                "representing trend in abundance over study period, a list of", 
                                "time series plots and the time series themselves")))
))

doEvent.splitModelPlot = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      sim <- scheduleEvent(sim, start(sim), "splitModelPlot", "fetchGIS")
      sim <- scheduleEvent(sim, start(sim), "splitModelPlot", "prediction")

      
    },
    fetchGIS = {

      if (!all(raster::res(sim$birdDensityRasters[[1]]) == c(30, 30))){
        templateRaster <- raster(resolution = c(30, 30), 
                                 crs = raster::crs(sim$birdDensityRasters[[1]]), 
                                 ext = extent(sim$birdDensityRasters[[1]]))
        sim$birdDensityRasters <- lapply(sim$birdDensityRasters, FUN = function(x){
          rasRes <- Cache(resample, x, templateRaster, method = "bilinear", filename = file.path(dataPath(sim), x@data@names), overwrite = TRUE)
          sim$rP <- sp::spTransform(sim$rP, CRSobj = raster::crs(x))
          maskedRasRes <- Cache(raster::mask, rasRes, sim$rP)
          return(maskedRasRes)
        })
      }

      sim$landCover <- prepInputs(targetFile = file.path(dataPath(sim), "CAN_NALCMS_LC_30m_LAEA_mmu12_urb05.tif"), 
                                  destinationPath = dataPath(sim), 
                                  rasterToMatch = sim$birdDensityRasters[[1]],
                                  studyArea = sim$rP)
      sim$landCover[] <- round(sim$landCover[], 0)
      raster::dataType(sim$landCover) <- "INT1U"
      
      sim$disturbanceType <- prepInputs(targetFile = file.path(dataPath(sim), "C2C_change_type.tif"),
                                        destinationPath = dataPath(sim),
                                        rasterToMatch = sim$birdDensityRasters[[1]],
                                        studyArea = sim$rP,
                                        quick = TRUE)
      sim$disturbanceType[] <- round(sim$disturbanceType[], 0)
      raster::dataType(sim$disturbanceType) <- "INT1U"

      sim$disturbanceYear <- prepInputs(targetFile = file.path(dataPath(sim), "C2C_change_year.tif"),
                                        destinationPath = dataPath(sim),
                                        rasterToMatch = sim$birdDensityRasters[[1]],
                                        studyArea = sim$rP,
                                        quick = TRUE) # Keep the file in memory only if the file is small enough. 
      browser()
      sim$disturbanceYear[] <- round(sim$disturbanceYear[], 0)
      raster::dataType(sim$disturbanceYear) <- "INT1U"

    },
    prediction = {

      sim$populationTrends <- splitRasterAndPredict(inputSpecies = sim$inputSpecies,
                                                    models = sim$scaleModels,
                                                    birdDensityRasters = sim$birdDensityRasters,
                                                    disturbanceType = sim$disturbanceType,
                                                    disturbanceYear = sim$disturbanceYear,
                                                    landCover = sim$landCover,
                                                    pathData = dataPath(sim),
                                                    nx = P(sim)$nx,
                                                    ny = P(sim)$ny,
                                                    buffer = P(sim)$buffer,
                                                    rType = P(sim)$rType,
                                                    start = start(sim),
                                                    end = end(sim),
                                                    forestClass = P(sim)$forestClass,
                                                    focalDistance = P(sim)$focalDistance,
                                                    disturbanceClass = P(sim)$disturbanceClass,
                                                    intermPath = cachePath(sim))
    },

    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if (!suppliedElsewhere("birdSpecies", sim)){
    sim$inputSpecies <- list("BBWA", "BLPW",
                      "BOCH", "BRCR",
                      "BTNW", "CAWA",
                      "CMWA","CONW",
                      "OVEN", "PISI",
                      "RBNU", "SWTH",
                      "TEWA", "WETA",
                      "YRWA")
  }
  
  if (!suppliedElsewhere('birdSpecies', sim)) {
    sim$birdSpecies <- c("BBWA", "BLPW", "BOCH", "BRCR", 
                         "BTNW", "CAWA", "CMWA", "CONW", 
                         "OVEN", "PISI", "RBNU", "SWTH", 
                         "TEWA", "WETA", "YRWA")
  }
  
  if (suppliedElsewhere("birdSpecies", sim) & !is(sim$birdSpecies, "list")){
    sim$inputSpecies <- as.list(sim$birdSpecies)
  } else {
      if (suppliedElsewhere("birdSpecies", sim) & is(sim$birdSpecies, "list")){
      sim$inputSpecies <- sim$birdSpecies
      }
    }
  
  if (!suppliedElsewhere("birdDensityRasters", sim)){
    stop("No bird density rasters were provided. Please provide these.")
  }
  
  if (!suppliedElsewhere("models", sim)){
    stop("Models not supplied. Please supply these.")
  }
  
  if(!is.null(P(sim)$testArea) & P(sim)$testArea == TRUE){
    sim$polyMatrix <- matrix(c(-93.028935, 50.271979), ncol = 2)
    sim$areaSize <- 50000
    set.seed(1234)
    sim$rP <- randomPolygon(x = polyMatrix, hectares = areaSize)
    message("Test area is TRUE. Cropping and masking to an area in south Ontario.")
  } else {
    sim$rP <- NULL
  }
  
  return(invisible(sim))
}
