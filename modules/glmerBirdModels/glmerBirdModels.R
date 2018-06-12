

defineModule(sim, list(
  name = "glmerBirdModels",
  description = c("The present module is based on the work of Suárez-Esteban et al. (in prep). These models investigate the following questions: 1. Do cumulative disturbances at local scales have a consistent effect across Canada on old forest-associated songbirds? 2. Do neighborhood disturbances influence the abundance of birds at local scales? 3. What are the relative effects of permanent and transitional disturbance agents at both scales?. For more information: Alberto Suárez-Esteban, Steve G. Cumming, Erin M. Bayne, Samantha J. Song, and Fiona K. A. Schmiegelow. The rise of industrial development, the fall of boreal songbirds: Industrial development threatens boreal songbirds across Canada"),
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
  reqdPkgs = list("data.table", "rgdal", "raster", "sf", "lme4", "googledrive"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("cropping", "logical", FALSE, NA, NA, "If the rasters should be cropped to a study area or not"),
    defineParameter("cropForModel", "logical", FALSE, NA, NA, "If the bird data should be cropped to a study area or not for fitting the model"),
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "logical", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = c("birdData", "studyArea", "typeDisturbance", "disturbanceDimension", 
                                "birdSpecies", "dataName", "studyAreaName"), 
                 objectClass = c("data.table","character", "character", "character", "character", "character", "character"), 
                 desc = c("Bird data assembled by the BAM (Boreal Avian Modelling Project)",
                          "Character to define the are to crop",
                          "Might be Transitional, Permanent, Undisturbed, and/or Both",
                          "Might be local and/or neighborhood",
                          "List of bird species to be modeled",
                          "File name and extension of original data file",
                          "Name of the file and extension to crop for study area"),
                 sourceURL = c(NA,NA,NA,NA,NA,NA, NA))
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = c("models","data","plotDistSec", "plotCoeff", "plotList", "plotAbundDist", "tableSampling", "AIC", "combinations"), #"studyArea"
                  objectClass = c("list","list","plot","plot", "data.table", "plot", "data.table", "data.table", "character"), #,"shapefile"
                  desc = c("list of boreal bird models", #"shapefile of the study area",
                           "list of the data already subsetted for the models",
                           "Plot of disturbance sectors",
                           "Plot of coefficients of bird models",
                           paste0("Data table of all models estimates, and std.error",
                                  " of % of disturbance per species, type and dimension of disturbance"),
                           "Plot of relative abundance per disturbance type, dimension and species on varying disturbance proportions",
                           "Table with the number of samples for each type and dimension",
                           "Table with all AIC values for all models",
                           "total combination of type and dimension of disturbances"))
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
      sim <- scheduleEvent(sim, start(sim), "glmerBirdModels", "plots")
      sim <- scheduleEvent(sim, start(sim), "glmerBirdModels", "save")
    },
    
    #REVIEW ARGUMENTS TO FUNCTIONS!!
    
    dataUploading = {
      
      ifelse (params(sim)$glmerBirdModels$cropForModel==TRUE,{
        sim$data <- sim$birdData
      },{
        sim$data <- dataUploading(data = sim$dataName, 
                                  combinations =  sim$combinations)})
    },
    birdModels = {
      
      sim$models <- Cache(birdModelsFunctionUpdated, combinations = sim$combinations, 
                                       dataset = sim$data,
                                       birdSp = sim$birdSpecies)
      
    },
    plots = {

      sim$plotDistSec <- plotDisturbanceSector2(sim = sim, 
                                               dataset = sim$data, 
                                               types = sim$typeDisturbance,
                                               outputPath = outputPath(sim))
      
      sim$plotList <- plotList(dataset = sim$models, 
                               combinations = sim$combinations, 
                               birdSp = sim$birdSpecies,
                               outputPath = outputPath(sim))
      
      sim$plotCoeff <- plotCoefficients3(sim = sim, 
                                        plotList = sim$plotList,
                                        outputPath = outputPath(sim))
      
      sim$plotAbundDist <- plotAbundanceDisturbance3(sim = sim, 
                                                    plotList = sim$plotList,
                                                    outputPath = outputPath(sim))
      
    },
    save = {
      
      sim$tableSampling <- tableSampling(sim = sim, 
                                         dataName = sim$dataName, 
                                         dataset = sim$data,
                                         outputPath = outputPath(sim))
      
      sim$AIC <- tableAIC(sim = sim, 
                          models = sim$models, 
                          birdSp = sim$birdSpecies, 
                          combinations = sim$combinations,
                          outputPath = outputPath(sim))
      
    },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  
  sim$models <- list()
  sim$combinations <- expand.grid(sim$disturbanceDimension, sim$typeDisturbance) %>%
    apply(MARGIN = 1, FUN = function(x) paste0(x[1],x[2]))
  
  return(invisible(sim))
}

.inputObjects = function(sim) {
  if (params(sim)$glmerBirdModels$cropForModel==TRUE){
    sim$studyArea <- loadStudyArea(data = "testArea.shp")
    sim$birdData <- loadCroppedData(sim = sim, dataName = "Final_points_BEAD_final.csv")
  }
   if (!suppliedElsewhere(sim$birdSpecies)){
    sim$birdSpecies <- c("BBWA", "BLPW", "BOCH", "BRCR", 
                         "BTNW", "CAWA", "CMWA", "CONW", 
                         "OVEN", "PISI", "RBNU", "SWTH", 
                         "TEWA", "WETA", "YRWA")}
  
  if (!suppliedElsewhere(sim$typeDisturbance)){
    sim$typeDisturbance = c("Transitional", "Permanent", "Both")
  }

  if (!suppliedElsewhere(sim$disturbanceDimension)){
    sim$disturbanceDimension = c("local", "neighborhood", "LocalUndisturbed")
  }
  
return(invisible(sim))
}
