fetchData <- function(pathData = dataPath(sim), 
                      birdSp = sim$birdSpecies, 
                      studyArea = sim$rP,
                      extractFrom4kRasters = P(sim)$extractFrom4kRasters,
                      densityEstimatesURL = sim$densityEstimatesURL,
                      densityEstimatesFileName = sim$densityEstimatesFileName) {
  require(raster)
  require(data.table)
  
  if (extractFrom4kRasters == TRUE){
    dataRaster <- lapply(X = birdSp, FUN = function(x){
      ras <- prepInputs(targetFile = paste0(x, "_currmean.asc"),
                        archive = paste0(x, "_current.zip"),
                        url = paste0("https://s3-us-west-2.amazonaws.com/bam-databasin-climatechangepredictions/climatepredictions-ascii/",
                                     x, "_current.zip"),
                        destinationPath = pathData,
                        studyArea = studyArea)
    })
  } else {
    
    # Need to create a raster with the densities based on LCC, PROV and BCR
    # 1. Download data
    
    # ras <- Cache(prepInputs, targetFile = paste0(birdSp[[1]], "_currmean.asc"),
    #              archive = paste0(birdSp[[1]], "_current.zip"),
    #              url = paste0("https://s3-us-west-2.amazonaws.com/bam-databasin-climatechangepredictions/climatepredictions-ascii/",
    #                           birdSp[[1]], "_current.zip"),
    #              destinationPath = pathData,
    #              studyArea = studyArea)
    # templateRaster <- raster::raster(resolution = c(250, 250),
    #                                  crs = raster::crs(ras), 
    #                                  ext = extent(ras))

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
    densityEstimates <- Cache(prepInputs, targetFile = densityEstimatesFileName,
                              destinationPath = pathData, fun = "data.table::fread") %>%
      .[SPECIES %in% birdSp,]
    
    # 2. Rasterize (fasterize) both PROV and BCR.
    
    # =========== BCR 
    BCRsf <- sf::st_as_sf(BCR)
    rasBCR <- fasterize::fasterize(sf = BCRsf, raster = LCC05, field = "BCR")
    
    # ==========+ PROV
    PROVsf <- sf::st_as_sf(PROV)
    PROVsf$PROV <- as.numeric(PROVsf$PRUID)
    rasPROV <- fasterize::fasterize(sf = PROVsf, raster = LCC05, field = "PROV") # [ FIX ] Is rasterizing using something that not the BCR number!
    
    # provTable <- data.frame(ID = c("59", "24", "62", "11", "47", "60", "46", "35", "13", "61", "48", "10", "12"),
    #                         PRENAME = c("British Columbia", "Quebec", "Nunavut", "Prince Edward Island", 
    #                                      "Saskatchewan", "Yukon", "Manitoba", "Ontario", "New Brunswick", 
    #                                      "Northwest Territories", "Alberta","Newfoundland and Labrador", "Nova Scotia"))
    # 
    # browser() # To provTable, add a column with the Abbreviation; use this in PRO_BCR_LCC to pull the values for _D
    # provinces <- c("Alberta", "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador",
    #                 "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", "Prince Edward Island",
    #                 "Quebec", "Saskatchewan", "Yukon")
    # 
    #  provincesAcronyms <- sort(unique(densityEstimates$PROV))
    #  provincesAcronyms[6:7] <- c("NT", "NS")
    #  provTable2 <- data.frame(PROV = as.character(provincesAcronyms), PRENAME = as.character(provinces))
    #  PROVabb <- merge(provTable, provTable2)
    #  
    # PROVabb <- as.character(plyr::join(provTable, provTable2)[["PRENAME"]])
    
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
      cols <- c("LCC", "ID", "BCR", "D")
      densityEstimatesRed <- densityEstimates[SPECIES == x, ..cols]
      vals <- plyr::join(PROV_BCR_LCC, densityEstimatesRed)
      # Create raster
      spRas <- raster::raster(resolution = res(LCC05), crs = raster::crs(LCC05), ext = extent(LCC05)) %>%
        raster::setValues(vals$D)
        return(spRas)
      }
    )
    
    names(densityRasList) <- birdSp
    
   }

    # Make some magic with the density tables to transform it to a dataRaster
 
  # For log values in the raster # Steve said to do the logging of variables in the formula
  # logDataRaster <- lapply(X = dataRaster, FUN = function(x){
  #   raster::values(x) <- log(raster::values(x))
  #   return(x)
  # })

  return(densityRasList) # Return one raster per species in a names list
}
