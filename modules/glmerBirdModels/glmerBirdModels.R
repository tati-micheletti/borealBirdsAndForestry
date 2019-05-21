defineModule(sim, list(
  name = "glmerBirdModels",
  description = c("The present module is based on the work of SuÃ¡rez-Esteban et al. (in prep). These models investigate the following questions: 1. Do cumulative disturbances at local scales have a consistent effect across Canada on old forest-associated songbirds? 2. Do neighborhood disturbances influence the abundance of birds at local scales? 3. What are the relative effects of permanent and transitional disturbance agents at both scales?. For more information: Alberto SuÃ¡rez-Esteban, Steve G. Cumming, Erin M. Bayne, Samantha J. Song, and Fiona K. A. Schmiegelow. The rise of industrial development, the fall of boreal songbirds: Industrial development threatens boreal songbirds across Canada"),
  keywords = c("songbirds", "forestry", "Anthropogenic disturbance"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Alberto", "Suarez-Esteban", email = "suarezes@ualberta.ca", role = c("aut"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0.9000", glmerBirdModels = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "glmerBirdModels.Rmd"),
  reqdPkgs = list("data.table", "rgdal", "raster", "sf", "lme4", "googledrive"),
  parameters = rbind(
    defineParameter("cropForModel", "logical", FALSE, NA, NA, "If the bird data should be cropped to a study area or not for fitting the model"),
    defineParameter("avoidAlbertosData", "logical", TRUE, NA, NA, "Should we use the most updated version of the BAM densities?"),
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("plot", "logical", FALSE, NA, NA, "Should plots be made?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "birdData", objectClass = "data.table", desc = "Bird data assembled by the BAM (Boreal Avian Modelling Project)", sourceURL = NA),
    expectsInput(objectName = "studyArea", objectClass = "character", desc = "Character to define the area to crop", sourceURL = NA),
    expectsInput(objectName = "typeDisturbance", objectClass = "character", desc = "Might be Transitional, Permanent, Undisturbed, and/or Both", sourceURL = NA),
    expectsInput(objectName = "disturbanceDimension", objectClass = "character", desc = "Might be local and/or neighborhood", sourceURL = NA),
    expectsInput(objectName = "birdSpecies", objectClass = "character", desc = "List of bird species to be modeled", sourceURL = NA),
    expectsInput(objectName = "dataName", objectClass = "character", desc = "File name and extension of original data file", sourceURL = NA),
    expectsInput(objectName = "studyAreaName", objectClass = "character", desc = "Name of the file and extension to crop for study area", sourceURL = NA),
    expectsInput(objectName = "birdDensityDS", objectClass = "data.table", desc = "list of data.table with information on species densities based on LCC and BCR/Prov", sourceURL = NA),
    expectsInput(objectName = "offsetsBySpecies", objectClass = "data.table", desc = "Offset values to correct for bird counts when using BAM dataset", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "models", objectClass = "list", desc = "list of boreal bird models"),
    createsOutput(objectName = "data", objectClass = "list", desc = "list of the data already subsetted for the models"),
    createsOutput(objectName = "plotDistSec", objectClass = "plot", desc = "Plot of disturbance sectors"),
    createsOutput(objectName = "plotCoeff", objectClass = "plot", desc = "Plot of coefficients of bird models"),
    createsOutput(objectName = "plotList", objectClass = "data.table", desc = paste0("Data table of all models estimates, and std.error",
                                                                                     " of % of disturbance per species, type and dimension of disturbance")),
    createsOutput(objectName = "plotAbundDist", objectClass = "plot", desc = "Plot of relative abundance per disturbance type, dimension and species on varying disturbance proportions"),
    createsOutput(objectName = "tableSampling", objectClass = "data.table", desc = "Table with the number of samples for each type and dimension"),
    createsOutput(objectName = "AIC", objectClass = "data.table", desc = "Table with all AIC values for all models"),
    createsOutput(objectName = "combinations", objectClass = "character", desc = "total combination of type and dimension of disturbances")
  )
))

## event types

doEvent.glmerBirdModels = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {

      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "glmerBirdModels", "dataUploading")
      sim <- scheduleEvent(sim, start(sim), "glmerBirdModels", "birdModels")
      if (P(sim)$plot == TRUE) {
        sim <- scheduleEvent(sim, start(sim), "glmerBirdModels", "plots")
      }
      sim <- scheduleEvent(sim, start(sim), "glmerBirdModels", "save")
    },
    
    dataUploading = {
      
      if (params(sim)$glmerBirdModels$cropForModel == TRUE) {
        sim$data <- sim$birdData
      } else {
        
        sim$SQLData <- suppressWarnings(retrieveSQLData(SQLtableVersion = sim$SQLtableVersion,
                                       SQLServer = sim$SQLServer,
                                       SQLDatabase = sim$SQLDatabase,
                                       birdSpecies = sim$birdSpecies))

        sim$data <- dataUploading(data = sim$dataName, 
                                  combinations =  sim$combinations,
                                  birdDensityDS = sim$birdDensityDS,
                                  offsetDT = sim$offsetsBySpecies,
                                  avoidAlbertosData = P(sim)$avoidAlbertosData,
                                  SQLTableName = "SQLData",
                                  envirSim = envir(sim))
browser()
        # Clean some of the data: Apparently in this new DS (29JAN19 - "Minidataset_master29JAN19.csv"), there are some 
        # really small "disturbances" in State_P_500 (10^-7) which are probably just
        # GIS overlay problems. They are classified as "" in Agent_500
        # Here they are converted to 0. Might help models' convergence
        rename <- names(sim$data)
        sim$data <- lapply(X = sim$data, FUN = function(ds){
          ds$State_P_100[ds[, State_P_100 > 0 & Agent_100 == ""]] <- 0
          ds$State_P_500[ds[, State_P_500 > 0 & Agent_500 == ""]] <- 0
          return(ds)
        })
        names(sim$data) <- rename
        }
    },
    birdModels = {
      
      if (P(sim)$avoidAlbertosData == FALSE) {
      sim$models <- Cache(birdModelsFunctionAlberto, combinations = sim$combinations,
                                       dataset = "data",
                                       birdSp = sim$birdSpecies,
                                       simEnv = envir(sim), userTags = "objectName:models")
    } else {
      sim$models <- Cache(birdModelsFunction, combinations = sim$combinations,
                          birdSp = sim$birdSpecies,
                          dataset = sim$data, userTags = "objectName:models")
    }
      
    },
    plots = {
      
      sim$plotDistSec <- plotDisturbanceSector(dataset = sim$data,
                                               types = sim$typeDisturbance,
                                               outputPath = mod$saveOut)
      
      sim$plotList <- plotList(dataset = sim$models,
                               combinations = sim$combinations,
                               birdSp = sim$birdSpecies,
                               outputPath = mod$saveOut)
      
      sim$plotCoeff <- plotCoefficients(plotList = sim$plotList,
                                        outputPath = mod$saveOut)
      
      sim$plotAbundDist <- plotAbundanceDisturbance(plotList = sim$plotList,
                                                    outputPath = mod$saveOut)
      
    },
    save = {
      
      sim$tableSampling <- tableSampling(dataName = sim$dataName,
                                         dataset = sim$data,
                                         outputPath = mod$saveOut)

      sim$AIC <- tableAIC(models = sim$models,
                          birdSp = sim$birdSpecies,
                          combinations = sim$combinations,
                          outputPath = mod$saveOut)
    },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  
  # Create a specific output folder
  mod$saveOut <- reproducible::checkPath(file.path(outputPath(sim),
                                                   toupper(format(Sys.time(), "%d%b%y"))),
                                         create = TRUE)
  
  sim$models <- list()
  sim$combinations <- expand.grid(sim$disturbanceDimension, sim$typeDisturbance) %>%
    apply(MARGIN = 1, FUN = function(x) paste0(x[1],x[2]))
  
  return(invisible(sim))
}

.inputObjects = function(sim) {
  if (params(sim)$glmerBirdModels$cropForModel == TRUE){
    sim$studyArea <- loadStudyArea(data = "testArea.shp")
    sim$birdData <- loadCroppedData(sim = sim, dataName = "Final_points_2010.csv")
  }
  if (!suppliedElsewhere("birdSpecies", sim)){
    sim$birdSpecies <- c("BBWA", "BLPW", "BOCH", "BRCR", 
                         "BTNW", "CAWA", "CMWA", "CONW", 
                         "OVEN", "PISI", "RBNU", "SWTH", 
                         "TEWA", "WETA", "YRWA")}
  
  if (!suppliedElsewhere(sim$typeDisturbance)){
    sim$typeDisturbance = c("Transitional", "Permanent", "Both")
  }
  
  if (!suppliedElsewhere("disturbanceDimension", sim)){
    sim$disturbanceDimension = c("local", "neighborhood", "LocalUndisturbed")
  }

  return(invisible(sim))
}
