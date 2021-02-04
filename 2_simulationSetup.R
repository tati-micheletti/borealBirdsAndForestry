############################################
############################################
#   S i m u l a t i o n     S e t u p      #  
############################################
############################################


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ LAYERS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Prepares the following layers:
# rasterToMatch: based on the land cover layer. Needs to be loaded 
#                first to provide crs for sA, and then reloaded to
#                to be cropped to worked sA
# studyArea: the whole boreal. Needs to be loaded with RTM for crs, 
#            and then worked on (remove Non-Canada and water bodies)

reproducibleSHA <- "bcab40a1"

stepCacheTag <- c(paste0("cache:2_simulationSetup"))

LCC1985 <- "https://drive.google.com/file/d/1IW5fOgQf7036DjJDOENX_oQQmkkia5mG/view?usp=sharing"
LCC2015 <- "https://drive.google.com/file/d/1C6nYsALPgYQQI0uo0jaNY28NXijP-PF_/view?usp=sharing"

RTMfile <- file.path(Paths$inputPath, "processedRTM.tif")
if (!file.exists(RTMfile)){
  rasterToMatch <- reproducible::Cache(reproducible::prepInputs, 
                                       url = paste0(
                                         "https://opendata.nfis.org/downloads/",
                                         "forest_change/CA_forest_harvest_mask",
                                         "_year_1985_2015.zip"),
                                       targetFile = "CA_harvest_year_1985_2015.tif", 
                                       destinationPath = SpaDES.core::Paths$inputPath,
                                       filename2 = "originalRTMdisturbance",
                                       userTags = c("objectName:rasterToMatch", stepCacheTag,
                                                    "outFun:noCache", paste0("reproducibleCommit:",
                                                                             reproducibleSHA),
                                                    "goal:getCRS"),
                                       omitArgs = c("overwrite", "destinationPath"))
  
  studyArea <- reproducible::Cache(usefulFuns::defineStudyArea,
                                   testArea = TRUE,
                                   specificTestArea = "boreal",
                                   mapSubset = "Canada")
  
  studyArea <- Cache(reproducible::postProcess, studyArea, 
                     destinationPath = SpaDES.core::Paths$inputPath,
                     rasterToMatch = rasterToMatch,
                     filename2 = file.path(SpaDES.core::Paths$inputPath, 
                                           "studyAreaPostProcessed"),
                     userTags = c("objectName:studyArea",
                                  stepCacheTag,
                                  paste0("reproducibleCommit:", 
                                         reproducibleSHA),
                                  "goal:sA"),
                     omitArgs = c("destinationPath"))
  
  studyAreaSF <- sf::st_as_sf(studyArea)
  studyAreaSF$RTM <- 1
  
  rasterToMatch <- fasterize::fasterize(sf = studyAreaSF, 
                                        raster = rasterToMatch, 
                                        field = "RTM")
  
  rasterToMatch <- reproducible::postProcess(x = rasterToMatch,
                                             studyArea = studyArea,
                                             destinationPath = SpaDES.core::Paths$inputPath,
                                             filename2 = file.path(SpaDES.core::Paths$inputPath, 
                                                                   "rasterToMatchPostProcessed"),
                                             userTags = c(
                                               "objectName:RTM", "goal:cropping",
                                               stepCacheTag,
                                               paste0("reproducibleCommit:",
                                                      reproducibleSHA),
                                               
                                               "goal:sA"))
 
  writeRaster(x = rasterToMatch, filename = RTMfile, 
              format = "GTiff", datatype = "INT1U", 
              overwrite = TRUE)
  
  qs::qsave(x = studyArea, file = file.path(Paths$inputPath, 
                                            "processedStudyArea.qs"))
  
} else {
  rasterToMatch <- raster::raster(RTMfile)
  studyArea <- qs::qread(file.path(Paths$inputPath, 
                                   "processedStudyArea.qs"))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PREAMBLE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I need to have a preamble for dealing with the focal calculation so I can use this data also 
# to fit the birds models. This needs to be done on a yearly basis. 
# Then, once I have focal for all years, I can use those maps + bird data to fit
# the statistical models (using hierarchical models)
# I should run already the 3 scales: 100m, 500m annulus, 1000m annulus
disturbanceRasterFilePath <- file.path(Paths$inputPath, "disturbanceRasterProcessed.tif")
if (!file.exists(disturbanceRasterFilePath)){
  disturbanceRaster <- reproducible::Cache(reproducible::prepInputs,
                                           url = paste0(
                                             "https://opendata.nfis.org/downloads/",
                                             "forest_change/CA_forest_harvest_mask",
                                             "_year_1985_2015.zip"),
                                           targetFile = "CA_harvest_year_1985_2015.tif",
                                           destinationPath = SpaDES.core::Paths$inputPath,
                                           filename2 = file.path(Paths$inputPath, "disturbanceRasterProcessed"),
                                           studyArea = studyArea,
                                           rasterToMatch = rasterToMatch,
                                           userTags = c("objectName:disturbanceRaster",
                                                        stepCacheTag, paste0("reproducibleCommit:",
                                                                             reproducibleSHA),
                                                        "outFun:Cache", "goal:prepDisturbanceRas"),
                                           omitArgs = c("overwrite", "destinationPath"))
  writeRaster(disturbanceRaster, file.path(Paths$inputPath, "disturbanceRasterProcessed"), 
              format = "GTiff")
} else {
  disturbanceRaster <- raster::raster(disturbanceRasterFilePath)
}

LCC85FilePath <- file.path(Paths$inputPath, "LCC1985.tif")
if (!file.exists(LCC85FilePath)){
  LCC1985 <- reproducible::Cache(reproducible::prepInputs, url = LCC1985,
                                 destinationPath = SpaDES.core::Paths$inputPath,
                                 overwrite = TRUE,
                                 targetFile = "Mosaic_vlce_hmm_1985_NN.dat",
                                 fun = "raster::raster",
                                 filename2 = file.path(Paths$inputPath, "LCC1985"),
                                 studyArea = studyArea,
                                 rasterToMatch = rasterToMatch,
                                 userTags = c("objectName:rasterToMatch", stepCacheTag,
                                              "outFun:noCache", paste0("reproducibleCommit:",
                                                                       reproducibleSHA),
                                              "goal:LCC1985"),
                                 omitArgs = c("overwrite", "destinationPath"))
  
  writeRaster(LCC1985, file.path(Paths$inputPath, "LCC1985"), 
              format = "GTiff")
} else {
  LCC1985 <- raster::raster(LCC85FilePath)
}

LCC15FilePath <- file.path(Paths$inputPath, "LCC2015.tif")
if (!file.exists(LCC15FilePath)){
  LCC2015 <- reproducible::Cache(reproducible::prepInputs, url = LCC2015,
                                 destinationPath = SpaDES.core::Paths$inputPath,
                                 overwrite = TRUE,
                                 targetFile = "Mosaic_vlce_hmm_2015_NN.dat",
                                 fun = "raster::raster",
                                 filename2 = file.path(Paths$inputPath, "LCC2015"),
                                 studyArea = studyArea,
                                 rasterToMatch = rasterToMatch,
                                 userTags = c("objectName:rasterToMatch", stepCacheTag,
                                              "outFun:noCache", paste0("reproducibleCommit:",
                                                                       reproducibleSHA),
                                              "goal:LCC2015"),
                                 omitArgs = c("overwrite", "destinationPath"))
  
  writeRaster(LCC2015, file.path(Paths$inputPath, "LCC2015"), 
              format = "GTiff")
} else {
  LCC2015 <- raster::raster(LCC15FilePath)
}

# Cluster is probably not going to be necessary, but might have the PROVINCE and BCR!!!
# Needs to intersect with LCC

# HERE --> make BCR_PROV_LCC raster!!!

# clusterFilePath <- file.path(Paths$inputPath, "BCR_Prov_LCCRaster.tif")
# if (!file.exists(clusterFilePath)){
#   ecodistrict <- Cache(reproducible::prepInputs, 
#                        url = "https://drive.google.com/file/d/1fAesdNQDIeoNL3kir4j9lHoOZSKtmkb8/view?usp=sharing", 
#                      destinationPath = SpaDES.core::Paths$inputPath,
#                      rasterToMatch = rasterToMatch,
#                      studyArea = studyArea,
#                      filename2 = file.path(SpaDES.core::Paths$inputPath, 
#                                            "Ecodistrict"),
#                      userTags = c("objectName:ecodistrict",
#                                   stepCacheTag,
#                                   paste0("reproducibleCommit:", 
#                                          reproducibleSHA),
#                                   "goal:ecodistrict"),
#                      omitArgs = c("destinationPath"))
#   
#   ecodistrictSF <- sf::st_as_sf(ecodistrict)
#   ecodistrictRas <- fasterize::fasterize(sf = ecodistrictSF, 
#                                         raster = rasterToMatch, 
#                                         field = "ECODISTRIC")
#   postProcessNecessary <- tryCatch({
#     raster::stack(rasterToMatch, LCC1985, LCC2015, 
#                   disturbanceRaster, ecodistrictRas)
#     return(FALSE)
#     }, error = function(e) return(TRUE))
#   if (postProcessNecessary){
#     clusterRaster <- reproducible::postProcess(x = ecodistrictRas,
#                                                studyArea = studyArea,
#                                                destinationPath = SpaDES.core::Paths$inputPath,
#                                                filename2 = file.path(SpaDES.core::Paths$inputPath, 
#                                                                      "ecodistrictPostProcessed"),
#                                                userTags = c(
#                                                  "objectName:ecodistrictRas", 
#                                                  "goal:cropping",
#                                                  stepCacheTag,
#                                                  paste0("reproducibleCommit:",
#                                                         reproducibleSHA),
#                                                  
#                                                  "goal:eD"))
#   }
#     
#   writeRaster(clusterRaster, file.path(Paths$inputPath, "clusterRaster"), 
#               format = "GTiff")
# } else {
#   clusterRaster <- raster::raster(clusterFilePath)
# }

## Set simulation and module parameters
times <- list(start = 1985, 
              end = 2015, 
              timeunit = "year")

focalDistance <- list(local = c(1, 100), neighborhood = c(100, 500), landscape = c(500, 1000))
buffer <- round(max(unlist(focalDistance))/unique(res(disturbanceRaster)), 0) + 1

parameters <- list(
  "focalStatsCalculation" = list(
    "classesToExcludeInLCC" = c(0, 20, 31, 32, 33, 40, 50, 80, 81, 100), # Non-forested classes
    "nx" = 2,
    "ny" = 2,
    "buffer" = c(buffer, buffer),
    # Buffer to make sure that when rasters are slip, they won't have edge effects
    "focalDistance" = focalDistance,
    "cleanScratch" = scratchDir
  )
)

objects <- list(
  "disturbanceRaster" = disturbanceRaster,
  "studyArea" = studyArea,
  "rasterToMatch" = rasterToMatch,
  "LandCoverRaster" = LCC1985
)

## Simulation setup
disturbanceProcessed <- SpaDES.core::simInitAndSpades(times = times, 
                                                      params = parameters,
                                                      modules = list("focalStatsCalculation"),
                                                      paths =  Paths,
                                                      objects = objects,
                                                      debug = 1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ SIMULATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# After the preamble I need to fit the bird models and then predict from these


