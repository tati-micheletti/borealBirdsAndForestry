defineModule(sim, list(
  name = "predictBirds",
  description = paste0("This module is intended to predict bird density", 
                       " given a model (i.e. coming from glmerBirdModels) and ",
                       "a raster with covariates for prediction (i.e. focalCalculation)"),
  keywords = c("boreal birds", "prediction", "disturbances"), # c("insert key words here"),
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.2", predictBirds = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "predictBirds.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("focalDistance", "numeric", 100, NA, NA, 
                    paste0("The distance at which to compute focal statistics, in units of", 
                           " the input rastesr CRS. This will be used to ", 
                           "create a matrix with circular weights summing to 1)")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, 
                    paste0("Should this entire module be run with caching activated?",
                           " This is generally intended for data-type modules, where ",
                           "stochasticity and time are not relevant"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "focalYearList", objectClass = "list", 
                  desc = paste0("This list is structured in a way that the ",
                                "masked value passed is the first level of ",
                                "the list, which is composed of a full patched",
                                " raster with all merged tiles")),
    expectsInput(objectName = "birdSpecies", objectClass = "character", 
                 desc = "List of bird species to be modeled"),
    expectsInput(objectName = "models", objectClass = "list", 
                 desc = "list of boreal bird models"),
    expectsInput(objectName = "birdDensityRasters", objectClass = "list", 
                 desc = paste0("list of rasters with information",
                               " on species densities based on LCC and BCR/Prov")),
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "predictRas", objectClass = "list", 
                  desc = "List of years, which is a list of species")
  )
))

doEvent.predictBirds = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "predictBirds", "predictBirdsDensities")
    },
    predictBirdsDensities = {
      
      sim$predictRas[[paste0("Year", time(sim))]] <- Cache(predictDensities, birdSpecies = sim$birdSpecies,
                                                                      disturbanceRas = sim$focalYearList[[paste0("Year", time(sim))]],
                                                                      birdDensityRasters = sim$birdDensityRasters,
                                                                      currentTime = time(sim),
                                                                      models = sim$models,
                                                                      pathData = dataPath(sim), 
                                                           userTags = paste0("predicted", time(sim)))
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + 1, "predictBirds", "predictBirdsDensities")
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  if (!suppliedElsewhere("focalYearList", sim)){
    message(paste0("No disturbance list of rasters found.", 
                   "Using fake disturbance raster for years ", 
                   start(sim), ":", end(sim)))
    sim$focalYearList <- Cache(fakeFocalRasterYears, st = start(sim),
                                              ed = end(sim),
                                              res = c(250, 250),
                                              crsRas = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
    names(sim$focalYearList) <- paste0("Year", start(sim):end(sim))
  }
  if (!suppliedElsewhere("birdDensityRasters", sim)){
    if (suppliedElsewhere("birdSpecies", sim)){
      stop(paste0("Bird list supplied, but density rasters not.",
                  " Please either provide either both objects, or ",
                  "none of these two."))
    } else {
    message(paste0("No bird density raster found.", 
                   "Using fake density raster for year BBWA"))
    sim$birdDensityRasters <- list()
    suppressWarnings(dir.create(file.path(dataPath(sim), "tmp")))
    tmpDir <- file.path(dataPath(sim), "tmp/tempBirdDensityRas.tif")
    if (!file.exists(tmpDir)) {
     ras <- SpaDES.tools::gaussMap(raster::raster(xmn = -30^3, xmx = 30^3,
                                            ymn = -30^3, ymx = 30^3,
                                            resolution = c(250, 250)))
      raster::crs(ras) <- "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
      ys <- seq(from = 0.6, to = 0.9, by = 0.05)
      bff <- (max(ras[]) - min(ras[]))/length(ys)
      vecM <- numeric(length(ys))
      cnty <- 1
      mult <- 0
      for (i in seq(from = 1, to = length(ys)*3, by = 3)){
        vecM[i:(2+i)] <- c(bff*mult, bff*(mult+1), ys[cnty])
        mult <- mult + 1
        cnty <- cnty + 1
      }
      rclmat <- matrix(vecM, ncol=3, byrow=TRUE)
      ras <- raster::reclassify(x = ras, rcl = rclmat)
    raster::writeRaster(x = ras, filename = tmpDir, format = "GTiff", overwrite = TRUE)
    }
    sim$birdDensityRasters[["BBWA"]] <- tmpDir
    }
  }
  if (!suppliedElsewhere("models", sim)){
    message("No models list found. Using fake generalized linear model for bay-breasted warbler (BBWA)")
    data <- data.frame(structure(list(AB_BBWA = c(0L, 0L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 
                                                  0L, 1L, 1L, 0L, 1L, 0L, 0L, 0L, 2L, 0L), 
                                      State_P_100 = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
                                                      1, 1, 0.039601406, 0.039601406, 0.039601406, 
                                                      0.00896732), 
                                      OFFSET_BBWA = c(0.143327645, 0.111694189, 0.122903008, 0.166319377, 
                                                  0.172458391, 0.159933748, 0.146178298, 0.159437289, 
                                                  0.144909933, 0.080126266, 0.101474693, 0.134386042, 
                                                  0.066678816, 0.148570951, 0.130694053, 0.098123795, 
                                                  0.139348905, 0.15018566, 0.144155741), 
                                      logDENSITY_BBWA = c(-0.300024604, -0.300024604, -0.300024604, -0.300024604, -0.300024604, 
                                                          -0.300024604, -0.155355509, -0.155355509, -0.155355509, -0.300024604, 
                                                          -0.300024604, -0.300024604, -0.300024604, -0.300024604, -0.300024604, 
                                                          -0.300024604, -0.300024604, -0.300024604, -0.300024604)), 
                                 .Names = c("AB_BBWA", "State_P_100","OFFSET_BBWA", "logDENSITY_BBWA"), 
                                 class = "data.frame", row.names = c(NA, -19L)))
    sim$models <- glm(AB_BBWA ~ State_P_100 + logDENSITY_BBWA, 
                      offset = OFFSET_BBWA, 
                      family = 'poisson', 
                      data = data)
    if (!suppliedElsewhere("birdSpecies", sim)){
      message("No species list found. Using fake data for bay-breasted warbler (BBWA)")
      sim$birdSpecies <- "BBWA"
    }
  }
  return(invisible(sim))
}
