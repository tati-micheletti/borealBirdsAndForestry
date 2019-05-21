# Global script for the Backcasting Project REMODELED
# devtools::load_all("/mnt/data/Micheletti/reproducible") # For debugging
# devtools::install_github("tati-micheletti/reproducible", ref = "development") # Before PR's
# devtools::install_github("PredictiveEcology/reproducible", ref = "development") # After PR's are accepted [ from 4th Jan on ]

# If using parallel, to view messages (in BorealCloud): tail -f /mnt/data/Micheletti/borealBirdsAndForestry/cache/logParallelFocal

# Testing for install of GDAL / These scripts are temporarily in the 'inputs' folder. Should be pushed to 'pemisc' once it is working/passing again.
setwd("/mnt/data/Micheletti/borealBirdsAndForestry")
invisible(sapply(X = list.files(file.path(getwd(), "functions"), 
                                full.names = TRUE), FUN = source))
if (!isGDALInstalled()) message("GDAL was not found in your computer, please make sure you install it before running these modules.")
if (!length(Sys.which("unzip")) > 0) message("unzip was not found in your computer, please make sure you install it before running these modules.")

# We also need to set a 'scratch' temporary folder for raster (if our home folder doesn't have enough space)
maxMemory <- 5e+12
user <- pemisc::user()
scratchDir <- reproducible::checkPath(path = paste0("/mnt/tmp/rasterTMP/", user), create = TRUE)

#Here we check that the creation of the folder worked (we might have problems with writting access, only tested with my own user)
if(dir.create(scratchDir)) system(paste0("sudo chmod -R 777 /mnt/tmp/rasterTMP"), wait = TRUE) 
raster::rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, rasterTmpDir = scratchDir)
# Make sure all packages are updated
# update.packages(checkBuilt = TRUE)
# devtools::install_github("PredictiveEcology/LandR")
# devtools::install_github("PredictiveEcology/reproducible@development")
# devtools::install_github("PredictiveEcology/map")
# devtools::install_github("PredictiveEcology/quickPlot@development")
# devtools::install_github("PredictiveEcology/SpaDES.core@development")
# devtools::install_github("PredictiveEcology/SpaDES.tools@development")
# devtools::install_github("PredictiveEcology/pemisc@development")

library(SpaDES.core)
library(SpaDES.tools)
reproducible::Require(ggplot2)
reproducible::Require(ggalt)

workDirectory <- getwd()

Paths <- list(
  cachePath = file.path(workDirectory, "cache"),
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

# Set temp folder
tryCatch(library(unixtools),
  error = function(e) install.packages("unixtools", repos = 'http://www.rforge.net/'))
  unixtools::set.tempdir(file.path(dirname(getwd()), "tmp"))
  
options('spades.moduleCodeChecks' = FALSE)
options('reproducible.useNewDigestAlgorithm' = FALSE)
options("reproducible.cachePath" = Paths$cachePath)
SpaDES.core::setPaths(modulePath = Paths$modulePath, inputPath = Paths$inputPath, outputPath = Paths$outputPath, cachePath = Paths$cachePath)

# Check for any log leftovers
leftoverLogs <- list.files(Paths$cachePath, pattern = "logParallel")
if (length(leftoverLogs) != 0)
  unlink(file.path(Paths$cachePath, leftoverLogs))

## list the modules to use
modules <- list("birdDensityBCR_Prov_LCC", "loadOffsetsBAM", "glmerBirdModels")#"predictBirds", "birdDensityTrends")
# modules <- list("birdDensityBCR_Prov_LCC", "loadOffsetsBAM", "glmerBirdModels", "predictBirds", "birdDensityTrends")
#Complete set of modules: "birdDensityBCR_Prov_LCC", "loadOffsetsBAM", "glmerBirdModels", "prepTiles",
# "focalCalculation", "predictBirds", "birdDensityTrends", "finalRasterPlots"

## Set simulation and module parameters
times <- list(start = 1985, end = 2011, timeunit = "year") # Cada 16 anos levam em media 12 horas. TOMORROW NIGHT: 2001 - 2011
parameters <- list(
  birdDensityBCR_Prov_LCC = list(extractFrom4kRasters = FALSE,
                                 avoidAlbertosData = TRUE,
                                 testArea = TRUE),
  bayesianBirdModel = list(testArea = TRUE), # FALSE means using boral shapefile to crop and mask
  glmerBirdModels = list(cropForModel = FALSE,
                         avoidAlbertosData = TRUE,
                         plot = TRUE),
  prepTiles = list(testArea = TRUE, # Should a study area be used (i.e. boreal)?
                   nx = 3, # mult 7
                   ny = 3, # mult 3
                   useParallel = NULL,
                   rType = "INT1U",
                   buffer = c(1300,1300), # Buffer to make sure that when rasters are slip, they won't have edge effects
                   .useCache = FALSE), # Should it override module's .useCache?
  focalCalculation = list(recoverTime = 30,
                          resampledRes = 250,
                          focalDistance = 100, # To run for neighborhood, change to c(100, 500)
                          disturbanceClass = 2, # 2 = Forestry, 1 = Fire, 3 and 4 = low probability forestry and fire
                          forestClass = 1:6, # Forested area class in the land cover map. If changing to fire might need to be rethought. Or not...
                          useParallel = NULL, #"local", # Local parallel for 500m not working apparently
                          nNodes = 1), # "across" = across machines, "local" = only on local machine, "NULL" or anything else = no parallel
  predictBirds = list(useParallel = FALSE),
  birdDensityTrends = list(plotting = FALSE)
)

tryCatch(googledrive::drive_download(file = googledrive::as_id("1S4ryXaqp0ZdW_yPBuYtNSPW5Mp1ft3R5"), 
                                     path = file.path(getPaths()$modulePath, "glmerBirdModels/data/models.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
tryCatch(googledrive::drive_download(file = googledrive::as_id("1KoL6QzKqCiBUZ8i6O-llCiit0G2APWLI"), 
                                     path = file.path(getPaths()$modulePath, "glmerBirdModels/data/data.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
models <- readRDS(file = file.path(getPaths()$modulePath, "glmerBirdModels/data/models.rds"))
data <- readRDS(file = file.path(getPaths()$modulePath, "glmerBirdModels/data/data.rds"))
disturbancePredict <- "Transitional"
birdSpecies <- c("BBWA", # Bird species to run the models for
                "BLPW",
                "BOCH",
                "BRCR",
                "BTNW",
                "CAWA",
                "CMWA",
                "CONW",
                "OVEN",
                "PISI",
                "RBNU",
                "SWTH",
                "TEWA",
                "WETA",
                "YRWA"
)

predictModels <- subsetModels(birdSp = birdSpecies, disturbancePredict = disturbancePredict, 
                       prmt = parameters, models = models)

.objects <- list( # Possible to include 'rP' directly here as a shapefile!
  predictModels = predictModels,
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

clearPlot()

#file.remove("/mnt/storage/borealBirdsAndForestry/cache/logParallel")

## Simulation setup
borealBirds100 <- SpaDES.core::simInitAndSpades(times = times, params = parameters, 
                                                modules = modules, paths =  Paths,
                                                objects = .objects, debug = 2)

# reproducible::Require(googledrive)
# googledrive::drive_upload(file.path(getwd(), "outputs/models.rds"), 
#                           path = as_id("1exnvJfgiRdTLN-RGpqxmVfZcssnDvY5Z"))
# 
# googledrive::drive_upload(file.path(getwd(), "outputs/data.rds"), 
#                           path = as_id("1exnvJfgiRdTLN-RGpqxmVfZcssnDvY5Z"))
# To save the outputs
# localAUG16 <- as(mySimOut, "simList_")
# saveRDS(localAUG16, file.path(outputPath(mySimOut), "localAUG16.rds"))

# To load the outputs
# mySimOut <- readRDS(file.path(outputPath(mySimOut), "backcast_10July2018.rds"))
