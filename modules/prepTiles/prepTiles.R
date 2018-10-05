defineModule(sim, list(
  name = "prepTiles",
  description = "This module can be used to prepare big rasters for operations. This module will run at the first year of the simulations",
  keywords = c("big data", "rasters", "GIS operations"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Ian", "Eddy", email = "ian.eddy@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.2", prepTiles = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "prepTiles.Rmd"),
  reqdPkgs = list("crayon"),
  parameters = rbind(
    defineParameter("nx", "numeric", 2, 1, NA, "the number of tiles to split raster into, along horizontal axis"),
    defineParameter("ny", "numeric", 2, 1, NA, "the number of tiles to split raster into, along vertical axis"),
    defineParameter("rType", "character", "FLT4S", NA, NA, "pixel data type for splitRaster"),
    defineParameter("buffer", "numeric", 3, 0, NA, "the number of cells to buffer tiles during splitRaster. Measured in cells, not distance"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "urlRaster1", objectClass = "character", 
                 desc = paste0("URL for the first raster that will be prepared.",
                               " The default is the disturbance raster from",
                               " White and Wulder 2017 (the type layer).", 
                               "This raster will be used to prepare", 
                               " the next rasters urlRaster2/3 (reprojecting",
                               " and masking)."),
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Type.zip"),
    expectsInput(objectName = "urlRaster2", objectClass = "character", 
                 desc = paste0("URL for the first raster that will be used.",
                               " The default is the LCC2010 raster 30m",
                               " This raster will be used to prepare", 
                               " the next raster (masking)."),
                 sourceURL = "http://www.cec.org/sites/default/files/Atlas/Files/2010nalcms30m/can_landcover_2010_30m.rar"),
    expectsInput(objectName = "urlRaster3", objectClass = "character", 
                 desc = paste0("URL for the first raster that will be prepared.",
                               " The default is the disturbance raster from",
                               " White and Wulder 2017 (the year layer).", 
                               " This is the raster that will be tiled to be used"),
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Year.zip"),
    expectsInput(objectName = "specificTestArea", objectClass = "character", 
                 desc = "Specific test area to crop to: 'boreal', or a province english name"),
    expectsInput(objectName = "rP", objectClass = "SpatialPolygonDataFrame", 
                 desc = "Random polygon in Ontario for when testArea = TRUE"),
    expectsInput(objectName = "mapSubset", objectClass = "character", 
                 desc = "Subset of the map")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "listTilePaths", objectClass = "character", 
                  desc = paste0("Character listing the raster paths to the tiles from",
                                " Raster3"))
  )
))

doEvent.prepTiles = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # Check we have all the objects needed: urlRaster1:3. rP can be NULL
      if (is.null(sim$urlRaster1)) {
        if (is.null(sim$Raster1)) {
          stop(paste0("No url nor object for Raster1 provided ",
                      "and default has not loaded. Check module code."))
        }
      }
      if (is.null(sim$urlRaster2)) {
        if (is.null(sim$Raster2)) {
          stop(paste0("No url nor object for Raster2 provided ",
                      "and default has not loaded. Check module code."))
        }
      }
      if (is.null(sim$urlRaster3)) {
        if (is.null(sim$Raster3)) {
          stop(paste0("No url nor object for Raster3 provided ",
                      "and default has not loaded. Check module code."))
        }
      }
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "prepTiles", "prepareBigRasters")
      
    },
    prepareBigRasters = {
      
      # Prepare Raster1-3: it will override the Raster1-3 objects if these were provided, 
      # reprojecting/cropping/masking depending on the arguments passed to prepInputs
      
      message(crayon::yellow(paste0("Raster1 being prepared.",
                                    " This might take a few hours depending on",
                                    " the extent of the raster.")))
      browser()
      sim$Raster1 <- Cache(prepInputs, url = sim$urlRaster1,
                            targetFile = file.path(dataPath(sim),"C2C_change_type.tif"),
                            destinationPath = dataPath(sim),
                            studyArea = sim$rP,
                            length = TRUE, useCache = TRUE) # took length = TRUE out. Don't remember what it does! >< Put it back just in case...
      
      # =============== Meanwhile Alternative =========================
      # reprojRaster1 <- file.path(dataPath(sim), "reprojRaster1.tif")
      # if (!file.exists(reprojRaster1)){
      #   ras1 <- raster::raster(file.path(dataPath(sim),"C2C_change_type.tif"))
      #   rPPath <- file.path(dataPath(sim), "cutline.shp")
      #   cutline <- sp::spTransform(x = sim$rP, CRS = as.character(raster::crs(ras1)))
      #   writeOGR(obj = sim$rP, dsn = dataPath(sim), layer = "cutline", driver = "ESRI Shapefile")
      #   
      #   system(paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp "),
      #                 "-cutline ", rPPath, " ",
      #                 "-crop_to_cutline ",
      #                 "-multi ",
      #                 "-wo NUM_THREADS=35 ",
      #                 "-tr 30 30 ",
      #                 "-ot Byte ",
      #                 "-of GTiff ",
      #                 file.path(dataPath(sim),"C2C_change_type.tif"), " ",
      #                 reprojRaster1),
      #          wait = TRUE)
      # }
      # sim$Raster1 <- raster::raster(reprojRaster1)
      
      # =============== Meanwhile Alternative =========================
      
      message(crayon::yellow(paste0("Raster2 being prepared.",
                                    " This might take a few hours depending on",
                                    " the extent of the raster.")))
      sim$Raster2 <- prepInputs(url = sim$urlRaster2,
                            targetFile = file.path(dataPath(sim), ""),
                            destinationPath = dataPath(sim),
                            rasterToMatch = sim$Raster1,
                            studyArea = sim$rP,
                            length = TRUE)
      
      # =============== Meanwhile Alternative =========================
      # reprojRaster2 <- file.path(dataPath(sim), "reprojRaster2.tif")
      # if (!file.exists(reprojRaster2)){
      #   ras2 <- raster::raster(file.path(dataPath(sim),"CAN_NALCMS_LC_30m_LAEA_mmu12_urb05.tif"))
      #   rPPath <- file.path(dataPath(sim), "cutline.shp")
      #   cutline <- sp::spTransform(x = sim$rP, CRS = as.character(raster::crs(ras2)))
      #   writeOGR(obj = sim$rP, dsn = dataPath(sim), layer = "cutline", driver = "ESRI Shapefile")
      #   
      #   system(paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp "),
      #                 "-s_srs \"", as.character(raster::crs(sim$reprojRaster2)), "\"",
      #                 " -t_srs \"", as.character(raster::crs(sim$reprojRaster1)), "\"",
      #                 "-cutline ", rPPath, " ",
      #                 "-crop_to_cutline ",
      #                 "-multi ",
      #                 "-wo NUM_THREADS=35 ",
      #                 "-tr 30 30 ",
      #                 "-ot Byte ",
      #                 "-of GTiff ",
      #                 file.path(dataPath(sim),"CAN_NALCMS_LC_30m_LAEA_mmu12_urb05.tif"), " ",
      #                 reprojRaster2),
      #          wait = TRUE)
      # }
      # sim$Raster2 <- raster::raster(reprojRaster2)
      # =============== Meanwhile Alternative =========================
      
      raster::extent(sim$Raster2) <- raster::alignExtent(extent = raster::extent(sim$Raster2), object = sim$Raster1, snap = "near")
      if (!raster::compareRaster(sim$Raster2, sim$Raster1, extent = TRUE)){
        stop("Rasters don't align. Please debug it.")
      }
      
      message(crayon::yellow(paste0("Raster3 being prepared.",
                                    " This might take a few hours depending on",
                                    " the extent of the raster.")))
      sim$Raster3 <- prepInputs(url = sim$urlRaster3,
                            archive = file.path(dataPath(sim), "C2C_Change_Year.zip"),
                            destinationPath = dataPath(sim),
                            rasterToMatch = sim$Raster2,
                            studyArea = sim$rP, length = TRUE)
      
      # =============== Meanwhile Alternative =========================
      # reprojRaster3 <- file.path(dataPath(sim), "reprojRaster3.tif")
      # if (!file.exists(reprojRaster3)){
      #   ras1 <- raster::raster(file.path(dataPath(sim),"C2C_change_year.tif"))
      #   rPPath <- file.path(dataPath(sim), "cutline.shp")
      #   cutline <- sp::spTransform(x = sim$rP, CRS = as.character(raster::crs(ras3)))
      #   writeOGR(obj = sim$rP, dsn = dataPath(sim), layer = "cutline", driver = "ESRI Shapefile")
      #   
      #   system(paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp "),
      #                 "-cutline ", rPPath, " ",
      #                 "-crop_to_cutline ",
      #                 "-multi ",
      #                 "-wo NUM_THREADS=35 ",
      #                 "-tr 30 30 ",
      #                 "-ot Byte ",
      #                 "-of GTiff ",
      #                 file.path(dataPath(sim),"C2C_change_year.tif"), " ",
      #                 reprojRaster3),
      #          wait = TRUE)
      # }
      # sim$Raster3 <- raster::raster(reprojRaster3)
      
      # =============== Meanwhile Alternative =========================
      raster::extent(sim$Raster3) <- raster::alignExtent(extent = raster::extent(sim$Raster3), object = sim$Raster2, snap = "near")
      if (!raster::compareRaster(sim$Raster3, sim$Raster2, extent = TRUE)){
        stop("Rasters don't align. Please debug it.")
      }
      
      message(crayon::green("Big rasters prepared for splitting!"))
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "prepTiles", "tileRasters")
    },
    tileRasters = {
      
      # Now all rasterd need to be tiled using the paramethers provided nx and ny
      message(crayon::yellow(paste0("Splitting Raster1 tiles")))
      
      browser()
      # [ FIX ] TRY BRINGING TO MEMORY TO MAKE TILING QUICKER? IF DOESN'T WORK, FORGET IT! IF WORKS, REPEAT BELOW
      sim$Raster1[] <- round(sim$Raster1[], 0)
      storage.mode(sim$Raster1[]) = "integer"
      sim$Raster1 <- Cache(splitRaster, r = sim$Raster1, nx = sim$nx, ny = sim$ny, buffer = sim$buffer,  # Splitting landCover Raster, write to disk,
                           rType = sim$rType, path = file.path(cachePath(sim), "Raster1")) # override the original in memory
      gc()
      
      message(crayon::yellow(paste0("Splitting Raster2 tiles")))
      sim$Raster2 <- Cache(splitRaster, r = sim$Raster2, nx = sim$nx, ny = sim$ny, buffer = sim$buffer,  # Splitting landCover Raster, write to disk,
                           rType = sim$rType, path = file.path(cachePath(sim), "Raster2")) # override the original in memory
      gc()
      
      message(crayon::yellow(paste0("Splitting Raster3 tiles")))
      sim$Raster3 <- Cache(splitRaster, r = sim$Raster3, nx = sim$nx, ny = sim$ny, buffer = sim$buffer,  # Splitting landCover Raster, write to disk,
                           rType = sim$rType, path = file.path(cachePath(sim), "Raster3")) # override the original in memory
      gc()
      
      sim$rastersList <- list("Raster1" = sim$Raster1, 
                              "Raster2" = sim$Raster2, 
                              "Raster3" = sim$Raster3) # Splitted rasters' list
      
      message(crayon::green("All rasters tiled!"))
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  if (!suppliedElsewhere(object = "urlRaster1", sim = sim)) {
    if (!suppliedElsewhere(object = "Raster1", sim = sim)) {
      message("Neither its url or Raster1 were provided. Using default")
      sim$urlRaster1 <- extractURL(objectName = "urlRaster1", sim = sim)  
    }
  }
  
  if (!suppliedElsewhere(object = "urlRaster2", sim = sim)) {
    if (!suppliedElsewhere(object = "Raster2", sim = sim)) {
      message(crayon::yellow(paste0("Neither its url or Raster2 were provided. Using default.",
                                    " ATTENTION: The default url for this raster downloads a .rar file.",
                                    " prepInputs doesn't deal with .rar for now. Please unrar it",
                                    " manually once it is downloaded.")))
      sim$urlRaster2 <- extractURL(objectName = "urlRaster2", sim = sim)
      
    }
  }  
  
  if (!suppliedElsewhere(object = "urlRaster3", sim = sim)) {
    if (!suppliedElsewhere(object = "Raster3", sim = sim)) {
      message("Neither its url or Raster3 were provided. Using default.")
      sim$urlRaster3 <- extractURL(objectName = "urlRaster3", sim = sim)  
    }
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
          if (!is.null(sim$mapSubset)) {
            sArP <- Cache(prepInputs,
                          url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip",
                          targetFile = "gpr_000b11a_e.shp",
                          # Subsetting to specific Provinces
                          archive = "gpr_000b11a_e.zip",
                          destinationPath = dataPath(sim), 
                          userTags = "objectName:sArP") %>%
              raster::subset(PRENAME %in% sim$mapSubset)
            if (nrow(sArP@data) == 0) {
              stop(paste0("There is no Canadian Province called ",
                          sim$mapSubset,
                          ". Please provide a Canadian province name in English for subsetMap, ",
                          "or use 'NULL' (does not subset boreal)."))
            }
          } else {
            sArP <- NULL
          }
          message(crayon::yellow("Test area is TRUE. Cropping and masking to the Canadian Boreal."))
          sim$rP <- prepInputs(alsoExtract = "similar", # [ FIX ] Needs a URL to make it more reproducible!
                               archive = file.path(dataPath(sim), "BRANDT_OUTLINE_Dissolve.zip"),
                               targetFile = file.path(dataPath(sim), "BRANDT_OUTLINE_Dissolve.shp"),
                               studyArea = sArP,
                               destinationPath = dataPath(sim)
          )
        } else {
          if (!is.null(sim$specificTestArea)) {
            sim$rP <- Cache(prepInputs,
                            url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip",
                            targetFile = "gpr_000b11a_e.shp",
                            # Subsetting to a specific Province
                            archive = "gpr_000b11a_e.zip",
                            destinationPath = dataPath(sim),
                            userTags = "objectName:rP") %>%
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
