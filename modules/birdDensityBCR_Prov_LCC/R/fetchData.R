fetchData <- function(pathData = dataPath(sim), 
                      birdSp = sim$birdSpecies, 
                      studyArea = sim$rP,
                      extractFrom4kRasters = P(sim)$extractFrom4kRasters,
                      densityEstimatesURL = sim$densityEstimatesURL,
                      densityEstimatesFileName = sim$densityEstimatesFileName,
                      avoidAlbertosData = P(sim)$avoidAlbertosData,
                      simEnv = envir(sim)) {
  require(raster)
  require(data.table)
  require(gdalUtils)
  
  if (extractFrom4kRasters == TRUE){
    dataRaster <- lapply(X = birdSp, FUN = function(x){
      ras <- prepInputs(targetFile = paste0(x, "_currmean.asc"),
                        archive = paste0(x, "_current.zip"),
                        url = paste0("https://s3-us-west-2.amazonaws.com/bam-databasin-climatechangepredictions/climatepredictions-ascii/",
                                     x, "_current.zip"),
                        destinationPath = pathData,
                        studyArea = studyArea, fun = "raster::raster")
    })
  } else {
    
      # Need to create a raster with the densities based on LCC, PROV and BCR
      # 1. Download data
      
      if (!file.exists(file.path(pathData, "LandCoverOfCanada2005_V1_4.zip"))){
        
        invisible(readline(prompt=paste("Make dure you have the dataset 'LandCoverOfCanada2005_V1_4.zip' in Google Drives folder", 
                                        "'BAM/Datasets/borealBirdsAndForestry', and press [enter] to continue",
                                        "\nIf authentication fails, please manually place the dataset file in the folder: \n",
                                        "~borealBirdsAndForestry/modules/birdDensityBCR_Prov_LCC/data")))
        require(googledrive)
        
        drive_download(file.path("BAM/Datasets/borealBirdsAndForestry", "LandCoverOfCanada2005_V1_4.zip"), 
                       path = file.path(pathData, "LandCoverOfCanada2005_V1_4.zip"), 
                       overwrite = FALSE,
                       verbose = FALSE)
      }
      
      LCC05 <- Cache(prepInputs, targetFile = "LCC2005_V1_4a.tif",
                     archive = "LandCoverOfCanada2005_V1_4.zip",
                     destinationPath = pathData,
                     studyArea = studyArea,
                     userTags = "objectName:LCC05")

      LCC05 <- Cache(projectInputs, LCC05, targetCRS = "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",
                     userTags = "objectName:LCC05")
      
      BCR <- Cache(prepInputs, url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip", 
                   targetFile = "BCR_Terrestrial_master.shp",
                   archive = "bcr_terrestrial_shape.zip",
                   destinationPath = pathData,
                   studyArea = studyArea,
                   rasterToMatch = LCC05,
                   userTags = "objectName:BCR")
      
      # For some reason it doesn't work for the boreal shapefile. However, BCR has province information. We will use that. 
      # But have to crop BCR to Canada as we don't have the densities for the USA. Asked Peter about it already, though.
      # PROV <- Cache(prepInputs, url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip",
      #               targetFile = "gpr_000b11a_e.shp",
      #               archive = "gpr_000b11a_e.zip",
      #               destinationPath = pathData,
      #               studyArea = studyArea,
      #               rasterToMatch = LCC05)
      
      if (!file.exists(file.path(pathData, densityEstimatesFileName))){
        stop(paste0("To date, the prepInputs() can't automatically download the density file needed for the analysis from Google Drive. Please manually download it at ",
                    densityEstimatesURL, " and place it in ", pathData))
      }
      
      # [ FIX ] Works if you have the file. Need to be fixed to be able to download the file # Double check if that is actually true...
      densityEstimates <- Cache(prepInputs, targetFile = densityEstimatesFileName, # OBS. Not all species have densities for all combinations of LCC_PROV_BCR
                                destinationPath = pathData, fun = "data.table::fread",
                                userTags = "objectName:densityEstimates") %>% # Also checked if it was pooled with other BCR, but not.
        .[SPECIES %in% birdSp,]
      
      if (avoidAlbertosData == TRUE){
        assign(x = "birdDensityDS", value = densityEstimates, envir = simEnv)
      }
      
      densityFiles <- as.list(paste0(pathData, "/density", birdSp, ".tif"))
      names(densityFiles) <- birdSp
      
      if (all(file.exists(unlist(densityFiles, use.names = FALSE)))){
        message(crayon::green(paste0("Density rasters exist, returning paths.")))
        return(densityFiles)
        
      } else {
        
        message(crayon::yellow(paste0("Not all density rasters exist. Creating..."))) # [ FIX ] only create the ones that don't exist, otherwise problems with rewritting!
        
      # 2. Rasterize (fasterize) both PROV and BCR.
      
      # =========== BCR 
      message(crayon::yellow(paste0("Fasterizing BCRsf")))
      BCRsf <- sf::st_as_sf(BCR)
      BCRsf <- BCRsf[!is.na(BCRsf$PROVINCE_S),] # Excluding a weird NA from the polygons
      BCRsf <- BCRsf[!BCRsf$COUNTRY == "USA",] # Excluding the USA from the analysis
      rasBCR <- Cache(fasterize::fasterize, sf = BCRsf, raster = LCC05, field = "BCR",
                      userTags = "objectName:rasBCR")
      rasBCR[] <- rasBCR[]
      
      # # ==========+ PROV
      
      PROV_ID <- data.table(PROVINCE_S = unique(BCRsf$PROVINCE_S), ID = seq(200, 199 + length(unique(BCRsf$PROVINCE_S))))
      BCRsf$PROV_ID <- PROV_ID$ID[match(BCRsf$PROVINCE_S, PROV_ID$PROVINCE_S)]
      rasPROV <- Cache(fasterize::fasterize, sf = BCRsf, raster = LCC05, field = "PROV_ID",
                       userTags = "objectName:rasPROV")
      
      # # ==========+ PROV
      # PROVsf <- sf::st_as_sf(PROV)
      # PROVsf$PROV <- as.numeric(PROVsf$PRUID)
      # message(crayon::yellow(paste0("Fasterizing PROVsf")))
      # rasPROV <- Cache(fasterize::fasterize, sf = PROVsf, raster = LCC05, field = "PROV")
      # rasPROV[] <- rasPROV[]
      
      # 3. Create a lookout table with dput FIX IT FOR THE SITUATION I HAVE NOW
      # Old ID for when I was downloading and rasterizing the province map: 
      # ID = structure(c(48L, 59L, 46L, 13L, 10L, 61L, 12L, 62L, 35L, 11L, 24L, 47L, 60L), class = "numeric"),
      
      PROVabb <- structure(list(PRENAME = structure(1:13, .Label = c("Alberta", 
                                                                     "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", 
                                                                     "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", 
                                                                     "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"), class = "factor"), 
                                ID = structure(c(207L, 206L, 200L, 210L, 201L, 202L, 211L, 203L, 209L, 212L, 
                                                 204L, 208L, 205L), class = "numeric"), 
                                PROV = structure(c(1L, 2L, 3L, 4L, 5L, 7L, 6L, 8L, 9L, 10L, 
                                                   11L, 12L, 13L), .Label = c("AB", "BC", "MB", "NB", "NL", 
                                                                              "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT"), class = "factor")), 
                           .Names = c("PRENAME", "ID", "PROV"), row.names = c(NA, -13L), class = "data.frame")
      
      # 4. Set a base data.table to retrieve the specific density values for each pixel 
      PROV_BCR_LCC <- data.table::data.table(ID = as.numeric(rasPROV[]),
                                             BCR = as.numeric(rasBCR[]),
                                             LCC = as.numeric(LCC05[]))
      
      densityEstimates <- plyr::join(densityEstimates, PROVabb)
      
      # 5. Lapply through the species and create a density raster at 250m for each species, 
      # write them to disk as 100m res, returning as the path to the list of rasters
      densityRasList <- lapply(X = birdSp, FUN = function(x) {
        invisible(gc())
        message(crayon::yellow(paste0("Creating density rasters for ", x)))
        cols <- c("LCC", "ID", "BCR", "D")
        densityEstimatesRed <- densityEstimates[SPECIES == x, ..cols]
        vals <- Cache(plyr::join, PROV_BCR_LCC, densityEstimatesRed,
                      userTags = "objectName:vals")
        # Create raster
        spRas <- raster::raster(resolution = res(LCC05), crs = raster::crs(LCC05), ext = extent(LCC05)) %>%
          raster::setValues(vals$D)
        spRas@data@names <- x
        # Write density files to disk while 
        raster::writeRaster(x = spRas, filename = densityFiles[[x]], overwrite = FALSE)
        rm(spRas)
        invisible(gc())
        return(densityFiles[[x]])
      }
      )
      
      names(densityRasList) <- birdSp
      ClassFilter <- function(x) inherits(get(x), "RasterLayer") & !inherits(get(x), "sf")
      rasRM <- Filter(ClassFilter, ls())
      rm(rasRM)
      invisible(gc())
    }
  }
  
  return(densityRasList) # Return one raster per species in a names list
}
