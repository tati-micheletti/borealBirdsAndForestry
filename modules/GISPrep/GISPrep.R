
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "GISPrep",
  description = NA, #"This module downloads, reprojects, and clips raster data representing historical forest disturbances in Canada",
  keywords = NA, # c("raster project clip disturbance"),
  authors = person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9004", GISPrep = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "GISPrep.Rmd"),
  reqdPkgs = list("magrittr", "raster", "rgdal", "gdalUtils"),
  parameters = rbind(
    defineParameter(".useCache", "numeric", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant") 
  ),
    inputObjects = bind_rows(
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataFrame", 
                 desc = 'shapefile to clipp and reproject raster inputs, default is New Brunswick', 
                 sourceURL = "http://geonb.snb.ca/downloads/wmz/geonb_wmz-zaf_shp.zip"), 
    expectsInput(objectName = "disturbanceYear", objectClass = "RasterLayer", 
                 desc = "A raster layer representing year of disturbance occurence", 
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Year.zip"),
    expectsInput(objectName = "disturbanceType", objectClass = "RasterLayer", 
                 desc = "A raster layer representing type of forest disturbance", 
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Type.zip")
  ),
    outputObjects = bind_rows(
    createsOutput(objectName = "disturbanceType_clip", objectClass = "RasterLayer", desc = "A raster layer representing fire and harvest disturbances clipped to study area"),
    createsOutput(objectName = "disturbanceYear_clip", objectClass = "RasterLayer", desc = "A raster layer representing the year a disturbance event occurred clipped to study area")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.GISPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- prepRasters(sim)

      # schedule future event(s)
      
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "GISPrep", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
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
prepRasters <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  if(!raster::compareCRS(studyArea, typeRaster)) {
    gdalUtils::gdalwarp() ####FIX THIS BIG GUY
  } else {
    outfile <- file.path(getwd(), "cache/disturbanceType_clip.tif")
    gdalUtils::gdalwarp(srcfile = typeRaster, dstfile = )
    sim$disturbanceType <- raster::raster(outfile)
  }
  
  
  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for your event1

.inputObjects <- function(sim) {
  #check for existence of input object rasters, download and unzip if necessary  
  if (is.null(sim$disturbanceType)){
    checksums1 <- downloadData("disturbanceType", modulePath(sim), overwrite = FALSE)
    result1 <- checksums1[checksums1$expectedFile == "C2C_Change_Type.tif",]$result
    
    if (result1 != "OK" | is.na(result1)) {
      file.name <- grep("C2C_Change_Type.tif$",
                        unzip(file.path(modulePath(sim), "GISPrep", "data", "C2C_Change_Type.tif"), list = TRUE)$Name,
                        value = TRUE)
      
      unzip(zipfile = file.path(modulePath(sim), "GISPrep", "data", "C2C_Change_Type.zip"),
            files = file.name,
            exdir = file.path(modulePath(sim), "GISPrep", "data"), junkpaths = TRUE)
      }}

  if (is.null(sim$disturbanceYear)){
    checksums1 <- downloadData("disturbanceYear", modulePath(sim), overwrite = FALSE)
    result1 <- checksums1[checksums1$expectedFile == "C2C_Change_Year.tif",]$result
    
    if (result1 != "OK" | is.na(result1)) {
      file.name <- grep("C2C_Change_Year.tif$",
                        unzip(file.path(modulePath(sim), "GISPrep", "data", "C2C_Change_Year.tif"), list = TRUE)$Name,
                        value = TRUE)
      
      unzip(zipfile = file.path(modulePath(sim), "GISPrep", "data", "C2C_Change_Year.zip"),
            files = file.name,
            exdir = file.path(modulePath(sim), "GISPrep", "data"), junkpaths = TRUE)
    }}
  
  #check for existence of study area shapefile, download one if necessary
  if (is.null(sim$studyArea)){
    checksums1 <- downloadData("studyArea", modulePath(sim), overwrite = FALSE)
    result1 <- checksums1[checksums1$expectedFile == "geonb_wmz-zaf.shp",]$result
    
    if (result1 != "OK" | is.na(result1)) {
      file.name <- grep("geonb_wmz-zaf.shp$",
                        unzip(file.path(modulePath(sim), "GISPrep", "data", "geonb_wmz-zaf.shp"), list = TRUE)$Name,
                        value = TRUE)
      
      unzip(zipfile = file.path(modulePath(sim), "GISPrep", "data", "geonb_wmz-zaf_shp.zip"),
            files = file.name,
            exdir = file.path(modulePath(sim), "GISPrep", "data"), junkpaths = TRUE)
    }}
  return(invisible(sim))
}