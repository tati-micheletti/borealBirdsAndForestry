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
                   studyArea = studyArea)
#    LCC05 <- projectInputs(LCC05, targetCRS = "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    LCC05[] <- LCC05[]
    
    
    BCR <- Cache(prepInputs, url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip", 
                 targetFile = "BCR_Terrestrial_master.shp",
                 archive = "bcr_terrestrial_shape.zip",
                 destinationPath = pathData,
                 studyArea = studyArea,
                 rasterToMatch = LCC05)
    
    PROV <- Cache(prepInputs, url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip", 
                       targetFile = "gpr_000b11a_e.shp",
                       archive = "gpr_000b11a_e.zip",
                       destinationPath = pathData,
                       studyArea = studyArea,
                       rasterToMatch = LCC05)
    
    if (!file.exists(file.path(pathData, densityEstimatesFileName))){
      stop(paste0("To date, the prepInputs() can't automatically download the density file needed for the analysis from Google Drive. Please manually download it at ",
                  densityEstimatesURL, " and place it in ", pathData))
    }

    # [ FIX ] Works if you have the file. Need to be fixed to be able to download the file
    densityEstimates <- Cache(prepInputs, targetFile = densityEstimatesFileName, # OBS. Not all species have densities for all combinations of LCC_PROV_BCR
                              destinationPath = pathData, fun = "data.table::fread") %>% # Also checked if it was pooled with other BCR, but not.
     .[SPECIES %in% birdSp,]
    
    if (avoidAlbertosData == TRUE){
      assign(x = "birdDensityDS", value = densityEstimates, envir = simEnv)
    }
    
    # 2. Rasterize (fasterize) both PROV and BCR.
    
    # =========== BCR 
    BCRsf <- sf::st_as_sf(BCR)
    message(crayon::yellow(paste0("Fasterizing BCRsf")))
    rasBCR <- Cache(fasterize::fasterize, sf = BCRsf, raster = LCC05, field = "BCR")
    rasBCR[] <- rasBCR[]
    
    # ==========+ PROV
    PROVsf <- sf::st_as_sf(PROV)
    PROVsf$PROV <- as.numeric(PROVsf$PRUID)
    message(crayon::yellow(paste0("Fasterizing PROVsf")))
    rasPROV <- Cache(fasterize::fasterize, sf = PROVsf, raster = LCC05, field = "PROV")
    rasPROV[] <- rasPROV[]
    
     # 3. Create a lookout table
    PROVabb <- structure(list(PRENAME = structure(1:13, .Label = c("Alberta", 
                                                        "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", 
                                                        "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", 
                                                        "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"), class = "factor"), 
                   ID = structure(c(48L, 59L, 46L, 13L, 10L, 61L, 12L, 62L, 35L, 11L, 
                                    24L, 47L, 60L), class = "numeric"), 
                   PROV = structure(c(1L, 2L, 3L, 4L, 5L, 7L, 6L, 8L, 9L, 10L, 
                                      11L, 12L, 13L), .Label = c("AB", "BC", "MB", "NB", "NL", 
                                                                 "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT"), class = "factor")), 
                   .Names = c("PRENAME", "ID", "PROV"), row.names = c(NA, -13L), class = "data.frame")
    
    # 4. Set a base data.table to retrieve the specific density values for each pixel 
    PROV_BCR_LCC <- data.table::data.table(ID = as.numeric(rasPROV[]),
                                           BCR = as.numeric(rasBCR[]),
                                           LCC = as.numeric(LCC05[]))
    
    densityEstimates <- plyr::join(densityEstimates, PROVabb)
    
    # 5. Lapply through the species and create a density raster at 250m for each species, returning as a names list of rasters
    densityRasList <- lapply(X = birdSp, FUN = function(x) {
      message(crayon::yellow(paste0("Creating density rasters for ", x)))
      cols <- c("LCC", "ID", "BCR", "D")
      densityEstimatesRed <- densityEstimates[SPECIES == x, ..cols]
      vals <- Cache(plyr::join, PROV_BCR_LCC, densityEstimatesRed)
      # Create raster
      spRas <- raster::raster(resolution = res(LCC05), crs = raster::crs(LCC05), ext = extent(LCC05)) %>%
        raster::setValues(vals$D)
        spRas@data@names <- x
        return(spRas)
      }
    )
    
    names(densityRasList) <- birdSp
    
  }


  return(densityRasList) # Return one raster per species in a names list
}
