
defineModule(sim, list(
  name = "bayesianBirdModel",
  description = NA, #"insert module description here",
  keywords = NA, # c("insert key words here"),
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9011", bayesianBirdModel = "0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "bayesianBirdModel.Rmd"),
  reqdPkgs = list("googledrive", "data.table", "raster", "stats", "gstat", "LandR","future", 
                  "future.apply", "stringr", "nimble", "tati-micheletti/usefun@development",
                  "ggplot2", "qs"),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("testArea", "logical", FALSE, NA, NA, "Should use study area?"),
    defineParameter("modelType", "numeric", 1, NA, NA, "Should use study area?"),
    defineParameter("fitFrequentistModel", "logical", TRUE, NA, NA, "Should run frequentist glmer model too?"),
    defineParameter("GDriveFolder", "character", NULL, NA, NA, "GDrive folder to upload model objects"),
    defineParameter("reRunModels", "logical", FALSE, NA, NA, "Should the bayesian and/or frequentist models be re-ran?"),
    defineParameter("useFuture", "logical", FALSE, NA, NA, "If lapply, should it use multicore future?"),
    defineParameter("plotBaysModel", "logical", TRUE, NA, NA, "Should plot bays models?"),
    defineParameter("savePredictedDT", "logical", FALSE, NA, NA, "Should save sim$predictedDT?"),
    defineParameter("numberGroups", "numeric", NA, NA, NA, "Should the species be divided into groups due to memory? If so, how many?"),
    defineParameter("overwriteBirdDensityDT", "logical", FALSE, NA, NA, "Should the density tables be overwritten?"),
    defineParameter("quickLoad", "logical", FALSE, NA, NA, paste0("Should the final density table be loaded? ",
                                                                  "This will error if the table doesn't yet exist ",
                                                                  "and needs to be set to FALSE if models need to be re-ran"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "birdDensityRasters", objectClass = "RasterStack", 
                 desc = "Raster stack of expected density of all birdSpecies", sourceURL = NA),
    expectsInput(objectName = "focalRasters", objectClass = "RasterStack", 
                 desc = "Raster stack of all disturbances 100 and 500m for all years", sourceURL = NA),
    expectsInput(objectName = "dataName", objectClass = "character", 
                 desc = "File name of used dataset", sourceURL = NA),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", 
                 desc = "Random polygon in Ontario for when testArea = TRUE", sourceURL = NA),
    expectsInput(objectName = "birdSpecies", objectClass = "character",
                 desc = "list of bird species to run the models for (same as Suarez 2019 et al.)", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "data", objectClass = "data.table", desc = "Bird dataset: Minidataset_master29JAN19.csv"),
    createsOutput(objectName = "fixedDT", objectClass = "list", desc = "Data.table of fixed values (model values) for the bayesian DT"),
    createsOutput(objectName = "yearDT", objectClass = "list", desc = "Data.table of yearly values (for prediction values) for the bayesian DT"),
    createsOutput(objectName = "hierarchicalModel", objectClass = "list", desc = "List of model types for each bird species"),
    createsOutput(objectName = "freqModels", objectClass = "list", desc = "List of frequentist models for each bird species"),
    createsOutput(objectName = "predictedDT", objectClass = "list", desc = "List of data table per species with predictions per pixel"),
    createsOutput(objectName = "coeff", objectClass = "list", desc = "List of full posterios from models per species"), 
    createsOutput(objectName = "dataForPrediction", objectClass = "list", desc = "List of data.table per species for predictions")
  ) 
))

doEvent.bayesianBirdModel = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim$freqModels <- sim$predictedDT <- list()
      if (any(nchar(start(sim)) < 4, nchar(end(sim)) < 4)) # Sanity check on years
        stop("This module deals with explictit years (1985 - 2011). Please provide the time in YYYY format.")
      
      sim$data <- prepInputs(url = "https://drive.google.com/open?id=1KoL6QzKqCiBUZ8i6O-llCiit0G2APWLI", 
                             fun = "base::readRDS", 
                             targetFile = "data.rds",
                             destinationPath = dataPath(sim))
      
      # schedule future events
      if (P(sim)$quickLoad){
        finalTablePath <- file.path(Paths$outputPath, "predictedDT.qs")
        if (file.exists(finalTablePath)){
          sim$predictedDT <- qs::qread(finalTablePath)
        } else {
          stop("Parameter quickLoad is TRUE, but file doesn't exist yeat. Please make a full run of the module first.")
        }
        sim$predictedDT <- file.path(Paths$outputPath, "predictedDT.qs")
      } else {
        sim <- scheduleEvent(sim, time(sim), "bayesianBirdModel", "model")
        sim <- scheduleEvent(sim, ifelse(P(sim)$plotBaysModel, time(sim), NA), "bayesianBirdModel", "plot")
        sim <- scheduleEvent(sim, time(sim), "bayesianBirdModel", "prediction")
      }
    },
    model = {
      message(paste0("Building the statistical data frame..."))
      sim$fixedDT <- dataframeBuilding(birdData = sim$data, birdSpecies = sim$birdSpecies)
   
      # Fit an equivalent frequentist model
      if (isTRUE(P(sim)$fitFrequentistModel)){
        library(lme4)
        sim$freqModels <- lapply(X = sim$birdSpecies, FUN = function(name){
          modelRDSpath <- file.path(Paths$outputPath, paste0("modelFreq_", name, ".rds"))
          if (file.exists(modelRDSpath) & !isTRUE(P(sim)$reRunModels)){
            suppressMessages(assign("mod", readRDS(modelRDSpath)))
            } else {
          suppressMessages(assign("mod", eval(parse(text = paste0("glmer(", name, " ~ State_P_500 + State_P_100 + logDENSITY_", name,
                                                                  " + (1|ClusterSP) + (1|YYYY)",
                                                                  ", offset = OFFSET_", name,
                                                                  ", family = 'poisson', data = sim$fixedDT[['", name,"']])")))))
          message("Fitting frequentist model to ", name, " finished")
            }
          return(mod)
      })
        names(sim$freqModels) <- sim$birdSpecies
        lapply(sim$birdSpecies, FUN = function(BIRD){
          saveRDS(sim$freqModels[[BIRD]], file.path(Paths$outputPath, 
                                                paste0("modelFreq_", BIRD,".rds")))
          if (!is.null(P(sim)$GDriveFolder))
            drive_upload(file.path(Paths$outputPath, paste0("modelFreq_", BIRD,".rds")),
                         as_id(P(sim)$GDriveFolder))
        })
      } else {
        lapply(sim$birdSpecies, FUN = function(BIRD){
          fileName <- file.path(Paths$outputPath, 
                                paste0("modelFreq_", BIRD,".rds"))
          if (file.exists(fileName)){
            sim$freqModels[[BIRD]] <- readRDS(fileName) 
          } else {
            sim$freqModels[[BIRD]] <- "File doesnt exist and was not created. Set the parameter fitFrequentistModel = TRUE"
          }
        })
      }
      # Fit the bayesian model
      message(paste0("Starting hierarchical model fitting for: ", paste(sim$birdSpecies, collapse = ", "), 
                     " for model(s) ", paste(P(sim)$modelType), collapse = " and "))
      lapplyFun <- ifelse(P(sim)$useFuture, future_lapply, lapply)
      if (P(sim)$useFuture)
      future::plan("multiprocess")
      sim$hierarchicalModel <- do.call(lapplyFun, args = list(X = seq_along(sim$birdSpecies),
                                               FUN = function(index){
                                                 predictHierarchicalModel(
                                                   bird = index,
                                                   birdList = sim$birdSpecies,
                                                   currentYearBirdData = sim$fixedDT[[index]],
                                                   currentTime = time(sim),
                                                   pathData = dataPath(sim),
                                                   modelType = P(sim)$modelType,
                                                   reRunModels = P(sim)$reRunModels,
                                                   useFuture = P(sim)$useFuture)
                                               }))
      names(sim$hierarchicalModel) <- sim$birdSpecies
      if (P(sim)$reRunModels){
        lapply(sim$birdSpecies, FUN = function(BIRD){
          lapply(P(sim)$modelType, FUN = function(modType){
            saveRDS(sim$hierarchicalModel[[BIRD]][[modType]], file.path(Paths$outputPath, 
                                                                        paste0("modelBay", modType, "_", BIRD,".rds")))
            if (!is.null(P(sim)$GDriveFolder))
              drive_upload(file.path(Paths$outputPath, paste0("modelBay", modType, "_", BIRD,".rds")),
                           as_id(P(sim)$GDriveFolder))
          })
        })
      }
    },
    plot = {
      if (length(P(sim)$modelType) == 1){
      dtSp <- rbindlist(lapply(sim$birdSpecies, FUN = function(BIRD){
          dtPlot <- sim$hierarchicalModel[[BIRD]][[paste0("model", P(sim)$modelType)]][["samples"]]
          dtPlot <- rbindlist(lapply(dtPlot, FUN = data.table))
          colsToRemove <- names(dtPlot)[!names(dtPlot) %in% c("beta[1]", "beta[2]")]
          dtPlot[, (colsToRemove) := NULL]
          dtPlot$species <- BIRD
          return(dtPlot)
          }))
      p <- ggplot(data = dtSp, aes(x = `beta[2]`, y = `beta[1]`, color = species)) +
        geom_point(size = 2) +
        facet_wrap(facets = "species") +
        geom_vline(xintercept = 0, linetype = "dashed",
                                       color = "darkred") +
        geom_hline(yintercept = 0, linetype = "dashed",
                                       color = "darkred") +
        # theme_dark() +
        theme(legend.position = "none") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        labs(y = "Neighborhood coefficient", x = "Local coefficient")
      ggsave(filename = file.path(Paths$outputPath, 
                                  paste0("betaCoefficients_model", 
                                         P(sim)$modelType, ".png")), 
             plot = p)
      print(p)
    } else {
      warning("length(P(sim)$modelType) != 1). Not plotting")
    }

    },
    prediction = {
      message(paste0("Predicting from the statistical data frame..."))
      # Retrieving coefficients `beta`
      sim$coeff <- lapply(names(sim$hierarchicalModel), function(BIRD){
        coef <- sim$hierarchicalModel[[BIRD]][[paste0("model", P(sim)$modelType)]][["samples"]]
        coef <- rbindlist(lapply(coef, FUN = data.table))
        colsToRemove <- grepMulti(names(coef), patterns = "lambda|R|mu")
        coef[, (colsToRemove) := NULL]
        # fix names
        names(coef) <- paste0("beta", 1:NCOL(coef))
        return(coef)
      })
      names(sim$coeff) <- names(sim$hierarchicalModel)
      
      # Assert that the names(coeff) == sim$birdSpecies!!! Otherwise messes up the coefficients
      testthat::expect_equal(names(sim$coeff), sim$birdSpecies)
      
      
      # Select the year for the correct focal layers and order correctly
      focalLays <- grepMulti(x = names(sim$focalRasters), patterns = time(sim))
      focalLayers <- lapply(X = focalLays, function(ras){
        fc <- sim$focalRasters[[ras]]
        names(fc) <- if (grepl(names(sim$focalRasters[[ras]]), pattern = "100")) "State_P_100" else "State_P_500"
        return(fc)
      })
      names(focalLayers) <- lapply(focalLayers, names)
      focalStack <- raster::stack(focalLayers[["State_P_500"]], focalLayers[["State_P_100"]])
      
      # Preparing DT for prediction
      sim$dataForPrediction <- prepareDTforPrediction(currentTime = time(sim),
                                                      pathData = dataPath(sim),
                                                      focalRasters = focalStack,
                                                      birdDensityRasters = sim$birdDensityRasters,
                                                      overwriteBirdDensityDT = P(sim)$overwriteBirdDensityDT)

      # future::plan("multiprocess")
      if (!is.na(P(sim)$numberGroups)){
        groups <- divideInChunks(vectorToSplit = sim$birdSpecies, numberOfGroups = P(sim)$numberGroups)
      } else {
        groups <- list("group1" = sim$birdSpecies)
      }
      if (P(sim)$useFuture){
        funLapply <- future_lapply
      } else {
        funLapply <- lapply
      }
      sim$predictedDT[[paste0("Year", time(sim))]] <- lapply(groups, function(speciesInGroup){ # DON'T use future here!
        spGroup <- do.call(funLapply, list(X = speciesInGroup, # future_lapply
                                 FUN = function(sp){
                                   predictFromPosterior(
                                     currentTime = time(sim),
                                     pathData = dataPath(sim),
                                     species = sp,
                                     dataForPrediction = sim$dataForPrediction[[sp]],
                                     covPars = sim$coeff[[sp]])
                                 }))
        return(spGroup)
        names(spGroup) <- speciesInGroup
      })
      
      sim$predictedDT[[paste0("Year", time(sim))]] <- do.call(c, sim$predictedDT[[paste0("Year", time(sim))]])
      names(sim$predictedDT[[paste0("Year", time(sim))]]) <- sim$birdSpecies
      
      if (time(sim) == end(sim)){
        predDTarranged <- lapply(sim$birdSpecies, function(BIRD){
          arrangedList <- lapply(sim$predictedDT, `[[`, BIRD)
          # Assertion:
          testthat::expect_true(length(unique(unlist(lapply(arrangedList, NROW)))) == 1)
          invisible(lapply(arrangedList, function(i) setkey(i, pixelID)))
          arrangedList <- Reduce(function(...) merge(..., by = c("pixelID", "originalDensity")), arrangedList)
          names(arrangedList)[names(arrangedList) == "originalDensity"] <- "Year1984" # Converting to 1984 to be easier to calculate cumm changes
          return(arrangedList)
        })
        sim$predictedDT <- predDTarranged
        if (P(sim)$savePredictedDT)
          qs::qsave(sim$predictedDT, file.path(Paths$outputPath, "predictedDT.qs"))
      }
      sim <- scheduleEvent(sim, time(sim) + 1, "bayesianBirdModel", "prediction")
    },
    
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  if(!suppliedElsewhere("dataName", sim)){
    sim$dataName <- "Minidataset_master29JAN19.csv"
  }
  if (!suppliedElsewhere("focalYearList", sim)){
    allFiles <- list.files(dataPath(sim), full.names = TRUE)
    focalRastersPaths <- grepMulti(patterns = c("mergedFocal", ".tif"), x = allFiles)
    if (length(focalRastersPaths) == 0){
      preProcess(url = "https://drive.google.com/open?id=1OtSXUGspv8UzPahmBOMDthkFm88HY3OK",
                 archive = "mergedFocalYYYY-DDDResXXXm.zip",
                 destinationPath = dataPath(sim))
      allFiles <- list.files(dataPath(sim), full.names = TRUE)
      focalRastersPaths <- grepMulti(patterns = c("mergedFocal", ".tif"), x = allFiles)
    }
    
    sim$focalRasters <- raster::stack(lapply(focalRastersPaths, FUN = raster))
    
  } else {
    sim$focalRasters <- raster::stack(sim$focalYearList)
    warning("Is the focalRasters stack really a stack that has both 100 and 500m focal calculations?
            if not, please DO NOT provide the file name (i.e. do not add the module focalCalculations). 
            This will be fixed in future versions")
  }
  
  if (!suppliedElsewhere("rP", sim)){
    # Default = Canadian boreal
    sim$rp <- defineStudyArea(testArea = TRUE, 
                              specificTestArea = "boreal", 
                              mapSubset = "Canada")
  }
  
  if (!suppliedElsewhere("birdSpecies", sim)){
    sim$birdSpecies = c("BBWA", "BLPW", "BOCH", "BRCR",
                        "BTNW", "CAWA", "CMWA","CONW",
                        "OVEN", "PISI", "RBNU", "SWTH",
                        "TEWA", "WETA", "YRWA")
  }
  if (!suppliedElsewhere("birdDensityRasters", sim)){
    sim$birdDensityRasters <- raster::stack(lapply(X = sim$birdSpecies, FUN = function(sp){
      fileToLoad <- grepMulti(x = list.files(path = dataPath(sim), full.names = TRUE), 
                              patterns = c("density", sp, ".tif"))
      if (length(fileToLoad) == 0){
        preProcess(url = "https://drive.google.com/open?id=1Htxc5Wv-30B0nJGpIyKa711x8cfGFrac", 
                   destinationPath = dataPath(sim))
        fileToLoad <- grepMulti(x = list.files(path = dataPath(sim), full.names = TRUE), 
                                patterns = c("density", sp, ".tif"))
      }
      ras <- raster::raster(fileToLoad)
    })
    )
    names(sim$birdDensityRasters) <- sim$birdSpecies
  }
  
  return(invisible(sim))
}
