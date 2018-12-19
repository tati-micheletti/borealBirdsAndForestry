
defineModule(sim, list(
  name = "loadOffsetsBAM",
  description = "Thre present module loads the lastest offsets for BAM dataset from P. Solymos to be used for bird models",
  keywords = c("BAM", "offsets", "boreal birds", "Canada"),
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.0.9000", loadOffsetsBAM = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "loadOffsetsBAM.Rmd"),
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
    expectsInput(objectName = "urlOffsets", objectClass = "character", desc = "url for newest offsets' archive", sourceURL = "http://ftp.public.abmi.ca/species.abmi.ca/bam-cawa/offsets-v3_2017-04-19.zip"),
    expectsInput(objectName = "newestDataset", objectClass = "character", desc = "Identify which offset table is the newer in order to merge it to olderst", sourceURL = NA),
    expectsInput(objectName = "olderDataset", objectClass = "character", desc = "Identify which offset table is the oldest in order to update it", sourceURL = NA),
    expectsInput(objectName = "birdSpecies", objectClass = "character", desc = "Species for which we want the offsets calculated", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "offsetsFiles", objectClass = "list", desc = "List containing offset files from BAM dataset"),
    createsOutput(objectName = "offsetsBySpecies", objectClass = "data.table", desc = "Offset values to correct for bird counts when using BAM dataset")
  )
))

doEvent.loadOffsetsBAM = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      sim <- scheduleEvent(sim, start(sim), "loadOffsetsBAM", "loadingOffsets")
      sim <- scheduleEvent(sim, start(sim), "loadOffsetsBAM", "processingOffsets")

      # [ FIX ] Give the user the option of chosing between loading and processing the offsets or calculating for his/her own data.
      # Easy to do this using if statements around the scheduleEvents.
      
    },
    loadingOffsets = {

      sim$offsetFiles <- Cache(retrieveOffsets, urlOffsets = sim$urlOffsets, 
                                         pathData = dataPath(sim), 
                                         newestDataset = sim$newestDataset,
                                         olderDataset = sim$olderDataset, 
                                         userTags = "objectName:offsetFiles")
    },
    processingOffsets = {

      sim$offsetsBySpecies <- Cache(processOffsets, offsetFilesName = "offsetFiles", 
                                    envirSim = envir(sim),
                                    birdSpecies = sim$birdSpecies, 
                                    userTags = "objectName:offsetsBySpecies")

    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  if (!suppliedElsewhere("newestDataset", sim)){
    sim$newestDataset <- "offsets-v3-new-chunk_2017-05-26.Rdata"
    sim$olderDataset <- "offsets-v3_2017-04-19.Rdata"
  }

  if (!suppliedElsewhere("urlOffsets", sim)){
    sim$urlOffsets <- "http://ftp.public.abmi.ca/species.abmi.ca/bam-cawa/offsets-v3_2017-04-19.zip"
  }
  if (!suppliedElsewhere("birdSpecies", sim)){
    sim$birdSpecies <-   c("BBWA", "BLPW", "BOCH", "BRCR",
                          "BTNW", "CAWA", "CMWA", "CONW",
                          "OVEN", "PISI", "RBNU", "SWTH",
                          "TEWA", "WETA","YRWA")
  }
    
  return(invisible(sim))
}
