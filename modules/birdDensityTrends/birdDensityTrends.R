defineModule(sim, list(
  name = "birdDensityTrends",
  description = paste0("This module will take a list of yearly predictions, fit a regression ", 
                       "model to these and return a list of slope and significancy per species"),
  keywords = c("trend", "linear model, "),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3", birdDensityTrends = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "birdDensityTrends.Rmd"),
  reqdPkgs = list("raster", "parallel"),
  parameters = rbind(
    defineParameter("plotting", "logical", TRUE, NA, NA, paste0("This describes if the ", 
                    "plotting event should occur")),
    defineParameter("useParallel", "logical", FALSE, NA, NA, paste0("Use parallel")),
    defineParameter(".useCache", "logical", FALSE, NA, NA, 
                    paste0("Should this entire module be",
                           " run with caching activated? This is generally intended for data-type", 
                           " modules, where stochasticity and time are not relevant"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "nCores", objectClass = "character",
                 desc = paste0("Number of cores to use for parallel. Possible values: auto, numeric, NULL")),
    expectsInput(objectName = "birdSpecies", objectClass = "character", 
                 desc = "List of bird species to be modeled"),
    expectsInput(objectName = "focalDistance", objectClass = "numeric",
                 desc = paste0("If provided somewhere else as a parameter, ", 
                               "the focal distance is inputted here as an object mainly for naming purposes. ",
                               "If the parameter is not provided, it becomes NULL")),
    expectsInput(objectName = "predictRas", objectClass = "list",
                 desc = "List of years, which is a list of species with density rasters")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "trends", objectClass = "list", 
                  desc = paste0("List of species, with significant slope raster. ",
                                "The slope has was masked by significancy at alpha = 0.05"))
  )
))

doEvent.birdDensityTrends = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- scheduleEvent(sim, end(sim), "birdDensityTrends", "fitTrend", eventPriority = .last())
      sim <- scheduleEvent(sim, end(sim), "birdDensityTrends", "plot", eventPriority = .last())
    },
    fitTrend = {
      sim$trends <- Cache(trendPerSpecies, birdSpecies = sim$birdSpecies,
                                    focalDistance = sim$focalDistance,
                                    predictRas = sim$predictRas,
                                    startTime = start(sim),
                                    useParallel = P(sim)$useParallel,
                                    nCores = sim$nCores, 
                                    endTime = end(sim),
                                    outPath = cachePath(sim),
                                    userTags = paste0("trendYears", sim$focalDistance,"TS:",
                                                      start(sim), ":",
                                                      end(sim)))
    },
    plot = {
      if (P(sim)$plotting == TRUE){
        lapply(X = names(sim$trends), FUN = function(sp){
          assign(x = paste0("ras", sp), value = raster::raster(sim$trends[[sp]]))
          eval(parse(text = paste0("quickPlot::Plot(ras", sp,", title = paste0('Trends from ', 
                                   start(sim), ' to ', end(sim), ' for ', sp))")))
        })
      } else {
        message(crayon::blurred("Plotting is turned off."))
      }
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  if (!suppliedElsewhere("nCores", sim)){
    sim$nCores <- "auto"
  }
  
  if (!suppliedElsewhere("focalDistance", sim)){
    if (!is.null(unlist(sim@params,
                        use.names = FALSE)[grepl(pattern = "focalDistance", 
                                                 x = names(unlist(sim@params)))])){
      sim$focalDistance <- max(as.numeric(unlist(sim@params, 
                                                 use.names = FALSE)[grepl(pattern = "focalDistance", 
                                                                          x = names(unlist(sim@params)))]))
    }
    if (!is.finite(sim$focalDistance)) stop("focalDistance was not supplied. Please supply it via other module or in .inputObjects")
  }

  if (!suppliedElsewhere("predictRas", sim)){
    sim$predictRas <- lapply(start(sim):end(sim), function(year){
      if (is.null(sim$birdSpecies)){
        message(paste0("No species list found. Using fake data for bay-breasted", 
                       " warbler and canada warbler (BBWA and CAWA)"))
        sim$birdSpecies <- c("BBWA", "CAWA")
      }
      predictRasBird <- lapply(sim$birdSpecies, function(bird){
        ras <- file.path(inputPath(sim), paste0("predicted", bird, max(sim$focalDistance),
                                                "mYear", year, ".tif"))
        rasExits <- file.exists(ras)
        if (rasExits){
          return(ras)
        } else {
          return(NULL)
        }
      })
      names(predictRasBird) <- sim$birdSpecies
      return(predictRasBird)
    })
    names(sim$predictRas) <- paste0("Year", start(sim):end(sim))
    #   yr <- start(sim):end(sim)
    #   # Just need to rename the lists
    #   sim$predictRas <- lapply(X = 1:length(sim$predictRas), FUN = function(nRas){
    #     lapply(X = 1:length(sim$birdSpecies), FUN = function(nSp){
    #       sim$predictRas[[nRas]][[nSp]]@data@names <- paste0(sim$birdSpecies[nSp], "Year", yr[nRas])
    #       return(sim$predictRas[[nRas]][[nSp]])
    #     })
    #     names(sim$predictRas[[nRas]]) <- sim$birdSpecies
    #     return(sim$predictRas[[nRas]])
    #   })
    #   names(sim$predictRas) <- paste0("Year", yr)
    # }
  }

  if (is.null(sim$predictRas)){
    message(paste0("No bird density raster found.",
                   " Using fake time series rasters for BBWA and CAWA for years ",
                   start(sim), " to ", end(sim)))
    sim$predictRas <- Cache(createRandomRasterList, rastersPerList = length(sim$birdSpecies),
                            numberOfLists = length(start(sim):end(sim)),
                            returnPaths = FALSE,
                            splittedTiles = FALSE)
  }
  
  if (!suppliedElsewhere("birdSpecies", sim)){
    message("No species list found. Using species names provided by the predictRas object")
    sim$birdSpecies <- names(sim$predictRas)
  }
  
  return(invisible(sim))
}
