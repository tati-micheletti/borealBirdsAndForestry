fetchData <- function(pathData,
                      birdsRangeList,
                      birdSp, 
                      studyArea,
                      extractFrom4kRasters,
                      densityEstimatesURL,
                      densityEstimatesFileName,
                      avoidAlbertosData) {
  require(raster)
  require(data.table)
  require(gdalUtils)
  if (extractFrom4kRasters == TRUE){
    dataRaster <- lapply(X = birdSp, FUN = function(x){
      ras <- prepInputs(targetFile = paste0(x, "_currmean.asc"),
                        archive = paste0(x, "_current.zip"),
                        url = paste0("https://s3-us-west-2.amazonaws.com/bam-databasin-climatechangepredictions/climatepredictions-ascii/",
                                     x, "_current.zip"),
                        destinationPath = asPath(pathData),
                        studyArea = studyArea, fun = "raster::raster")
    })
  } else {
    
    LCC05 <- Cache(prepInputs, url = "https://drive.google.com/open?id=19rMA800ZFsKkXx-eqBcXdR84P9MQdCLX",
                   destinationPath = asPath(pathData),
                   studyArea = studyArea,
                   overwrite = TRUE,
                   userTags = "objectName:LCC05")
    targetCRS <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    
    LCC05 <- Cache(projectInputs, LCC05, targetCRS = targetCRS, userTags = "objectName:LCC05reproj")
    
    BCR <- Cache(prepInputs, url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip", 
                 targetFile = "BCR_Terrestrial_master.shp",
                 alsoExtract = "similar",
                 destinationPath = asPath(pathData),
                 studyArea = studyArea,
                 rasterToMatch = LCC05,
                 overwrite = TRUE,
                 userTags = "objectName:BCR")

    if (!file.exists(file.path(asPath(pathData), densityEstimatesFileName)))
      message(crayon::green(paste0("prepInputs() is downloading the density file needed for the analysis from Google Drive and placing in ",
                                 pathData)))
    densityEstimates <- Cache(prepInputs, url = "https://drive.google.com/open?id=1SEcJdS25YkIoRMmrgGNe4-HKG90OtYjX",
                              targetFile = densityEstimatesFileName, # OBS. Not all species have densities for all combinations of LCC_PROV_BCR. Not all species are everywhere!
                              destinationPath = asPath(pathData), fun = "data.table::fread",
                              userTags = "objectName:densityEstimates", overwrite = TRUE) %>% # Also checked if it was pooled with other BCR, but not.
      .[SPECIES %in% birdSp,]
    
    if (avoidAlbertosData == TRUE){
      birdDensityDS <- densityEstimates
    } else {
      birdDensityDS <- NULL
      }
    
    densityFiles <- as.list(paste0(pathData, "/density", birdSp, ".tif"))
    names(densityFiles) <- birdSp
    
    if (all(file.exists(unlist(densityFiles, use.names = FALSE)))){
      message(crayon::green(paste0("Density rasters exist, returning paths.")))
      return(list(densityRasList = densityFiles, birdDensityDS = birdDensityDS))
    } else {
      message(crayon::red(paste0("Not all density rasters exist. Creating the missing ones...")))
      densityRasList <- lapply(X = birdSp, FUN = function(sp){
        if (file.exists(densityFiles[[sp]])) {
          return(densityFiles[[sp]])
        } else {
          BCR_PROV_LCC_Estimates <- Cache(createBCR_PROV_LCC_Estimates, BCR = BCR, LCC05 = LCC05, 
                                          densityEstimates = densityEstimates,
                                          userTags = "objectName:BCR_PROV_LCC_Estimates")
          BCR_PROV_LCC_Estimates$PROV_BCR_LCC$pixelID <- seq_len(NROW(BCR_PROV_LCC_Estimates$PROV_BCR_LCC))
          invisible(gc())
          message(crayon::yellow(paste0("Creating density rasters for ", sp)))
          cols <- c("LCC", "ID", "BCR", "D")
          densityEstimatesRed <- BCR_PROV_LCC_Estimates$densityEstimates[SPECIES == sp, ..cols]
          setkey(BCR_PROV_LCC_Estimates$PROV_BCR_LCC, LCC, ID, BCR)
          setkey(densityEstimatesRed, LCC, ID, BCR)
          vals <- densityEstimatesRed[BCR_PROV_LCC_Estimates$PROV_BCR_LCC]
          rm(BCR_PROV_LCC_Estimates)
          invisible(gc())
          setkey(vals, pixelID)
          # Create raster
          spRas <- raster::raster(resolution = res(LCC05), crs = raster::crs(LCC05), ext = extent(LCC05)) %>%
            raster::setValues(vals$D)
          spRas@data@names <- sp
          rm(vals)
          invisible(gc())
          
          #Mask raster birds range birdsRangeList
          spRas <- maskingSpeciesRange(densityRasters = spRas,
                                       sp = sp, pathData = pathData,
                                       studyArea = studyArea,
                                       birdsRangeList = birdsRangeList,
                                       filenameRas = densityFiles[[sp]])
          invisible(gc())
          return(spRas)
        }
      })
      names(densityRasList) <- birdSp
      ClassFilter <- function(x) inherits(get(x), "RasterLayer") & !inherits(get(x), "sf")
      rasRM <- Filter(ClassFilter, ls()) # Cleaning up?
      rm(rasRM)
      invisible(gc())
    }
  }
  invisible(gc())
  return(list(densityRasList = densityRasList, birdDensityDS = birdDensityDS)) # Return one raster per species in a names list
}
