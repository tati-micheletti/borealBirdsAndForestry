
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
  reqdPkgs = list("googledrive", "data.table", "raster", "stats", "gstat"),
  parameters = rbind(
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("testArea", "logical", FALSE, NA, NA, "Should use study area?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "dataName", objectClass = "character", desc = "File name of used dataset", sourceURL = NA),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster", sourceURL = "ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif"),
    expectsInput(objectName = "beads", objectClass = "SpatialPolygonDataFrame", desc = "shapefile with forestry and bird count locations", sourceURL = "https://drive.google.com/open?id=1VSlDXiID7A-R9ryim0NYOAaxq7Cf00n_"),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", desc = "Random polygon in Ontario for when testArea = TRUE", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "birdData", objectClass = "data.table", desc = "Bird dataset"),
    createsOutput(objectName = "models", objectClass = "list", desc = "list of boreal bird models"),
    expectsInput(objectName = "ageMap", objectClass = "RasterLayer", desc = "Age class map raster"),
    expectsInput(objectName = "beads", objectClass = "SpatialPolygonDataFrame", desc = "shapefile with disturbances and years")
  )
))

doEvent.bayesianBirdModel = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # do stuff for this event
      sim$birdData <- datasetUploading(dataPath = dataPath(sim), data = sim$dataName)
      sim$beads <- prepInputs(targetFile = file.path(dataPath(sim), "BEAD_2000.shp"),
                              alsoExtract = c("BEAD_2000.dbf", "BEAD_2000.prj", "BEAD_2000.sbn", 
                                              "BEAD_2000.sbx", "BEAD_2000.shp", "BEAD_2000.shp.xml", "BEAD_2000.shx"),
                              url = "https://drive.google.com/open?id=1VSlDXiID7A-R9ryim0NYOAaxq7Cf00n_",
                              archive = "BEAD_2000.zip",
                              destinationPath = dataPath(sim), 
                              studyArea = sim$rP)
      
      sim$ageMap <- loadAndProcessAgeMap(dataPath = dataPath(sim), projection = sp::proj4string(sim$beads), rP = sim$rP)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "bayesianBirdModel", "model")
    },
    model = {
      
      sim$models <- bayesModel(birdData = sim$birdData, 
                               ageMap = sim$ageMap, 
                               beads = sim$beads,
                               birdSpecies = sim$birdSpecies,
                               dataPath = dataPath(sim),
                               rP = sim$rP)
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
  
  
  if (any(is.null(P(sim)$testArea), (!is.null(P(sim)$testArea) & P(sim)$testArea == FALSE))) {
    if (!is.null(sim$specificTestArea)){
      sim$rP <- NULL
      message(crayon::yellow(paste0(
        "Test area is FALSE or NULL, but specificTestArea is not. Ignoring 'specificTestArea' and running the analysis for the whole country. ", 
        "To set a study area, use testArea == TRUE.")))
    } else {
      sim$rP <- NULL
      message(crayon::yellow(
        "Test area is FALSE or NULL. Running the analysis for the whole country."))
    }
  } else {
    if (!suppliedElsewhere("rP", sim) &
        is.null(sim$specificTestArea)) {
      sim$polyMatrix <- matrix(c(-79.471273, 48.393518), ncol = 2) 
      sim$areaSize <- 10000000
      set.seed(1234)
      sim$rP <- randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon
      message(crayon::yellow(
        "Test area is TRUE, specificTestArea is 'NULL'. Cropping and masking to an area in south Ontario."))
    } else {
      if (!suppliedElsewhere("rP", sim) & 
          sim$specificTestArea == "boreal") {
        message(crayon::yellow(
          "Test area is TRUE. Cropping and masking to the Canadian Boreal."))
        sim$rP <- prepInputs(url = "http://cfs.nrcan.gc.ca/common/boreal.zip",
                             alsoExtract = "similar",
                             targetFile = file.path(dataPath(sim), "NABoreal.shp"), #Boreal Shapefile
                             destinationPath = dataPath(sim))
      } else {
        if (!suppliedElsewhere("rP", sim) &
            !is.null(sim$specificTestArea)){
          sim$rP <- Cache(prepInputs, url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip", 
                          targetFile = "gpr_000b11a_e.shp", # Subsetting to a specific Province
                          archive = "gpr_000b11a_e.zip",
                          destinationPath = asPath(dataPath(sim))) %>%
            raster::subset(PRENAME == sim$specificTestArea)
          if (nrow(sim$rP@data) == 0) {
            stop(paste0(
              "There is no Canadian Province called ", sim$specificTestArea, 
              ". Please provide a Canadian province name in English for specificTestArea, ", 
              "use 'boreal', or use 'NULL' (creates a random area in South Ontario)."))
          } else {
            message(crayon::yellow(paste0(
              "Test area is TRUE. Cropped and masked to ", sim$specificTestArea)))
            
          }
        }
      }
    }
  }
  
  return(invisible(sim))
}
