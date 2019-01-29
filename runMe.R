# Global script for the Backcasting Project REMODELED
# devtools::load_all("/mnt/data/Micheletti/reproducible") # For debugging
# devtools::install_github("tati-micheletti/reproducible", ref = "development") # Before PR's
# devtools::install_github("PredictiveEcology/reproducible", ref = "development") # After PR's are accepted [ from 4th Jan on ]

# If using parallel, to view messages (in BorealCloud): tail -f /mnt/data/Micheletti/borealBirdsAndForestry/cache/logParallelFocal

# Testing for install of GDAL / These scripts are temporarily in the 'inputs' folder. Should be pushed to 'pemisc' once it is working/passing again.
source(file.path(getwd(), "inputs", "isGDALInstalled.R"))
source(file.path(getwd(), "inputs", "defineStudyArea.R"))
if (!isGDALInstalled()) message("GDAL was not found in your computer, please make sure you install it before running these modules.")
if (!length(Sys.which("unzip")) > 0) message("unzip was not found in your computer, please make sure you install it before running these modules.")
invisible(sapply(X = list.files(file.path(getwd(), "functions"), full.names = TRUE), FUN = source))

# Make sure all packages are updated
devtools::install_github("PredictiveEcology/LandR")
devtools::install_github("PredictiveEcology/reproducible@development")
devtools::install_github("PredictiveEcology/map")
devtools::install_github("PredictiveEcology/quickPlot@development")
devtools::install_github("PredictiveEcology/SpaDES.core@development")
devtools::install_github("PredictiveEcology/SpaDES.tools@development")
devtools::install_github("PredictiveEcology/pemisc@development")

library(SpaDES.core)
library(SpaDES.tools)
reproducible::Require(ggplot2)
reproducible::Require(ggalt)

# Which computer is this being run on?
# Options are: BorealCloud, LocalMachine, 388
whichComputer <- "388"

paths <- pathsSetup(whichComputer = whichComputer, whichProject = "borealBirdsAndForestry", setTmpFolder = TRUE) 
# If setTmpFolder == TRUE, a temporary folder will be created inside the cache folder set. 
# This can/should be changed later for other projects using:
# ===> In Windows:
# write(TMPDIR = '<path/to/tempFolder>'), file = file.path(Sys.getenv('R_USER'), '.Renviron'))
# ===> In Linux/MacOS:
# tryCatch(library(unixtools),
#   error = function(e) install.packages("unixtools", repos = 'http://www.rforge.net/'))
#   unixtools::set.tempdir(<path/to/tempFolder>)
options('spades.moduleCodeChecks' = FALSE)
options('reproducible.useNewDigestAlgorithm' = FALSE)
options("reproducible.cachePath" = paths$cachePath)
SpaDES.core::setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)

# Check for any log leftovers
leftoverLogs <- list.files(paths$cachePath, pattern = "logParallel")
if (length(leftoverLogs) != 0)
  unlink(file.path(paths$cachePath, leftoverLogs))

## list the modules to use
modules <- list("birdDensityBCR_Prov_LCC", "loadOffsetsBAM", "glmerBirdModels")
#Complete set of modules: "birdDensityBCR_Prov_LCC", "loadOffsetsBAM", "glmerBirdModels", "prepTiles",
# "focalCalculation", "predictBirds", "birdAbundanceTrends", "finalRasterPlots

## Set simulation and module parameters
times <- list(start = 1985, end = 2011, timeunit = "year")
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
                          focalDistance = c(100, 500), # To run for neighborhood, change to c(100, 500)
                          disturbanceClass = 2, # 2 = Forestry, 1 = Fire, 3 and 4 = low probability forestry and fire
                          forestClass = 1:6, # Forested area class in the land cover map. If changing to fire might need to be rethought. Or not...
                          useParallel = NULL, #"local", # Local parallel for 500m not working apparently
                          nNodes = 1), # "across" = across machines, "local" = only on local machine, "NULL" or anything else = no parallel
  birdDensityTrends = list(plotting = FALSE)
)

objects <- list( # Possible to include 'rP' directly here as a shapefile!
  mapSubset = "Canada", # "Canada" or Provinces to run at once. Good to subset provinces still within the boreal
  specificTestArea = "boreal", # "boreal", or canadian provinces
  SQLtableVersion = "V4_2015", # Data retrieving from SQL: specific versions
  SQLServer = "boreal.biology.ualberta.ca", # Data retrieving from SQL: server
  SQLDatabase = "BAM_National_V4_2015_0206", # Data retrieving from SQL: specific database
  dataName = "Minidataset_master29JAN19.csv", # Alberto's manuscript data to select points and GIS. Data are, however coming from SQL.
  birdSpecies = c("BBWA", # Bird species to run the models for
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
  ),
  typeDisturbance = c("Transitional", "Permanent", "Both"), #, "Permanent", "Both"
  disturbanceDimension = c("local", "neighborhood", "LocalUndisturbed"), #, "neighborhood", "LocalUndisturbed"
  disturbancePredict = c("Transitional")#, # Needs to match disturbanceClass from prediction module. Type of disturbance we want to predict from.
  # Raster1 = NA, # direct path to locally stored object as a "character string.
  # urlRaster1 = NA, #If you want to download the object from a specific url that not the default, specify the url as a "character string" here
  # Use the same logic for Raster2 and Raster3
)

clearPlot()

#file.remove("/mnt/storage/borealBirdsAndForestry/cache/logParallel")

## Simulation setup
mySim <- SpaDES.core::simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
mySimOut <- SpaDES.core::spades(mySim, debug = TRUE)

# To save the outputs
# localAUG16 <- as(mySimOut, "simList_")
# saveRDS(localAUG16, file.path(outputPath(mySimOut), "localAUG16.rds"))

# To load the outputs
# mySimOut <- readRDS(file.path(outputPath(mySimOut), "backcast_10July2018.rds"))