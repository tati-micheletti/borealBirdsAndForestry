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
                   filename2 = "studyAreaRTMcrs",
                   userTags = c("objectName:studyArea",
                                stepCacheTag,
                                paste0("reproducibleCommit:", 
                                       reproducibleSHA),
                                "goal:sA"),
                   omitArgs = c("destinationPath"))

source(file.path(getwd(), 'functions/processRasterToMatch.R'))
rasterToMatch <- processRasterToMatch(RTMpath = file.path(Paths$inputPath, "rasterToMatch.tif"),
                                      rasterToMatch = rasterToMatch, 
                                      studyArea = studyArea, 
                                      reproducibleSHA = reproducibleSHA)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PREAMBLE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I need to have a preamble for dealing with the focal calculation. This needs to be done on a 
# yearly basis. Then, once I have focal for all years, I can use those maps + bird data to fit
# the statistical models (using hierarchical models)
# I should run already the 3 scales: 100m, 500m annulus, 1000m annulus

disturbanceRaster <- reproducible::Cache(reproducible::prepInputs,
                                         url = paste0(
                                           "https://opendata.nfis.org/downloads/",
                                           "forest_change/CA_forest_harvest_mask",
                                           "_year_1985_2015.zip"),
                                         targetFile = "CA_harvest_year_1985_2015.tif",
                                     destinationPath = SpaDES.core::Paths$inputPath,
                                     filename2 = "disturbanceRasterProcessed",
                                     studyArea = studyArea,
                                     rasterToMatch = rasterToMatch,
                                     userTags = c("objectName:disturbanceRaster",
                                                  stepCacheTag, paste0("reproducibleCommit:",
                                                                       reproducibleSHA),
                                                  "outFun:Cache", "goal:prepDisturbanceRas"),
                                     omitArgs = c("overwrite", "destinationPath"))

LCC1985 <- reproducible::Cache(reproducible::prepInputs, url = LCC1985,
                       destinationPath = SpaDES.core::Paths$inputPath,
                       overwrite = TRUE,
                       targetFile = "Mosaic_vlce_hmm_1985_NN.dat",
                       fun = "raster::raster",
                       filename2 = "LCC1985",
                       studyArea = studyArea,
                       rasterToMatch = rasterToMatch,
                       userTags = c("objectName:rasterToMatch", stepCacheTag,
                                    "outFun:noCache", paste0("reproducibleCommit:",
                                                             reproducibleSHA),
                                    "goal:LCC1985"),
                       omitArgs = c("overwrite", "destinationPath"))

LCC2015 <- reproducible::Cache(reproducible::prepInputs, url = LCC2015,
                               destinationPath = SpaDES.core::Paths$inputPath,
                               overwrite = TRUE,
                               targetFile = "Mosaic_vlce_hmm_2015_NN.dat",
                               fun = "raster::raster",
                               filename2 = "LCC2015",
                               studyArea = studyArea,
                               rasterToMatch = rasterToMatch,
                               userTags = c("objectName:rasterToMatch", stepCacheTag,
                                            "outFun:noCache", paste0("reproducibleCommit:",
                                                                     reproducibleSHA),
                                            "goal:LCC2015"),
                               omitArgs = c("overwrite", "destinationPath"))

## Set simulation and module parameters
times <- list(start = 1985, end = 2015, timeunit = "year")
parameters <- list(
  # "bayesianBirdModel" = list(
  #   "modelType" = 3,
  #   "GDriveFolder" = "1VfLUqH4Kkr5zZNwmRb4OjJzt_FrRiK8-",
  #   "fitFrequentistModel" = FALSE,
  #   "savePredictedDT" = TRUE,
  #   "plotBaysModel" = FALSE,
  #   "quickLoad" = FALSE
  # ),
  "prepTiles" = list(
    "testArea" = TRUE,
    # Should a study area be used (i.e. boreal)?
    "nx" = 3,
    # mult 7
    "ny" = 3,
    # mult 3
    "useParallel" = NULL,
    "rType" = "INT1U",
    "buffer" = c(1300, 1300),
    # Buffer to make sure that when rasters are slip, they won't have edge effects
    ".useCache" = FALSE
  ),
  # Should it override module's .useCache?
  "focalCalculation" = list(
    "recoverTime" = 30,
    "resampledRes" = 250,
    "focalDistance" = 100,
    # To run for neighborhood, change to c(100, 500)
    "disturbanceClass" = 2,
    # 2 = Forestry, 1 = Fire, 3 and 4 = low probability forestry and fire
    "forestClass" = 1:6,
    # Forested area class in the land cover map. If changing to fire might need to be rethought. Or not...
    "useParallel" = NULL,
    #"local", # Local parallel for 500m not working apparently
    "nNodes" = 1
    # "across" = across machines, "local" = only on local machine, "NULL" or anything else = no parallel
  )
)

# birdSpecies <- c( ) # Check from BAM's dataset, maybe not even pass...?

objects <- list( # Possible to include 'rP' directly here as a shapefile!
  predictModels = predictModels,
  focalDistance = 100, # Should be provided only if the trends module is the only one used
  # models = predictModels,
  data = data,
  mapSubset = "Canada", # "Canada" or Provinces to run at once. Good to subset provinces still within the boreal
  specificTestArea = "boreal", # "boreal", or canadian provinces
  SQLtableVersion = "V4_2015", # Data retrieving from SQL: specific versions
  SQLServer = "boreal.biology.ualberta.ca", # Data retrieving from SQL: server
  SQLDatabase = "BAM_National_V4_2015_0206", # Data retrieving from SQL: specific database
  dataName = "Minidataset_master29JAN19.csv", # Alberto's manuscript data to select points and GIS. Data are, however coming from SQL.
  birdSpecies = birdSpecies,
  typeDisturbance = c("Transitional", "Permanent", "Both"), #, "Permanent", "Both"
  disturbanceDimension = c("local", "neighborhood", "LocalUndisturbed"), #, "neighborhood", "LocalUndisturbed"
  disturbancePredict = disturbancePredict #, # Needs to match disturbanceClass from prediction module. Type of disturbance we want to predict from.
  # Raster1 = NA, # direct path to locally stored object as a "character string.
  # urlRaster1 = NA, #If you want to download the object from a specific url that not the default, specify the url as a "character string" here
  # Use the same logic for Raster2 and Raster3
)

## Simulation setup
birdsPrediction <- SpaDES.core::simInitAndSpades(times = times, 
                                                 params = parameters,
                                                 modules = modules, 
                                                 paths =  Paths,
                                                 objects = objects, 
                                                 debug = 1)
