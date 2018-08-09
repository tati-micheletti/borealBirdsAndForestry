
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects, use sim$xxx, and are thus globally available
# to all modules. Functions can be used without sim$ as they are namespaced, like functions
# in R packages. If exact location is required, functions will be: sim$<moduleName>$FunctionName
defineModule(sim, list(
  name = "finalRasterPlots",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.0.9000", finalRasterPlots = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "finalRasterPlots.Rmd"),
  reqdPkgs = list("raster", "sp", "viridis", "spatialEco", "sf"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated?"),
    defineParameter("testArea", "logical", FALSE, NA, NA, "Should use study area?")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "populationTrends", objectClass = "list", desc = "list per species of 1st (1985), last (2011), slope and significancy (p-value) of birs densities", sourceURL = NA),
    expectsInput(objectName = "specificTestArea", objectClass = "character", desc = "Specific test area to crop to: 'boreal', or a province english name", sourceURL = NA),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", desc = "Random polygon in Ontario for when testArea = TRUE", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "percDensityChange", objectClass = "list", desc = "List of final delta% change graphs per species"),
    createsOutput(objectName = "negativeTrendPercentage", objectClass = "plot", desc = "barPlot of percentage of signif negative trends per species per BCR")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.finalRasterPlots = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "finalRasterPlots", "plot")
      
    },
    plot = {

      sim$percDensityChange <- deltaAbundanceGraph(populationTrends = sim$populationTrends,
                                      studyArea = sim$rP,
                                      pathData = dataPath(sim),
                                      outputPath = outputPath(sim))
      
# TAKES TOO LONG! WILL RUN IT SEPARATELY AND INCORPORATE IN THE MODEL ONLY WHEN I HAVE SOMETHING FOR THE POSTER!
      # sim$negativeTrendPercentage <- createBarGraph(percDensityChange = sim$percDensityChange, 
      #                                               populationTrends = sim$populationTrends,
      #                                               studyArea = sim$rP,
      #                                               pathData = dataPath(sim),
      #                                               outputPath = outputPath(sim))
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects = function(sim) {
  
  if(!suppliedElsewhere("populationTrends", sim)){
    stop("Need to supply population trends list of graphs!")
  }
  
  
  if (!suppliedElsewhere("rP", sim)) {
    if (any(is.null(P(sim)$testArea), (!is.null(P(sim)$testArea) &
                                       P(sim)$testArea == FALSE))) {
      if (!is.null(sim$specificTestArea)) {
        sim$rP <- NULL
        message(crayon::yellow(paste0(
          "Test area is FALSE or NULL, but specificTestArea is not. Ignoring 'specificTestArea' and running the analysis for the whole country. ",
          "To set a study area, use testArea == TRUE.")))
      } else {
        sim$rP <- NULL
        message(crayon::yellow("Test area is FALSE or NULL. Running the analysis for the whole country."))
      }
    } else {
      if (is.null(sim$specificTestArea)) {
        sim$polyMatrix <- matrix(c(-79.471273, 48.393518), ncol = 2) 
        sim$areaSize <- 10000000
        set.seed(1234)
        sim$rP <- randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon
        message(crayon::yellow("Test area is TRUE, specificTestArea is 'NULL'. Cropping and masking to an area in south Ontario."))
      } else {
        if (sim$specificTestArea == "boreal") {
          message(crayon::yellow("Test area is TRUE. Cropping and masking to the Canadian Boreal."))
          sim$rP <- prepInputs(url = "http://cfs.nrcan.gc.ca/common/boreal.zip",
                               alsoExtract = "similar",
                               targetFile = file.path(dataPath(sim), "NABoreal.shp"),
                               #Boreal Shapefile
                               destinationPath = dataPath(sim)
          )
        } else {
          if (!is.null(sim$specificTestArea)) {
            sim$rP <- Cache(prepInputs,
                            url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip",
                            targetFile = "gpr_000b11a_e.shp",
                            # Subsetting to a specific Province
                            archive = "gpr_000b11a_e.zip",
                            destinationPath = dataPath(sim)) %>%
              raster::subset(PRENAME == sim$specificTestArea)
            if (nrow(sim$rP@data) == 0) {
              stop(paste0("There is no Canadian Province called ",
                          sim$specificTestArea,
                          ". Please provide a Canadian province name in English for specificTestArea, ",
                          "use 'boreal', or use 'NULL' (creates a random area in South Ontario)."))
            } else {
              message(crayon::yellow(paste0("Test area is TRUE. Cropped and masked to ",
                                            sim$specificTestArea)))
              
            }
          }
        }
      }
    }
  }
  
  return(invisible(sim))
}

