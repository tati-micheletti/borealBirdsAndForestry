# Global script for the Backcasting Project REMODELED

# OBS. on 26th Oct I installed back PredictiveEcology/SpaDES.tools 
# ref = 42a02a10b61cf5dfef557d5c50e60477fc3ab74c (Which includes my changes to mergeRaster)
# OBS. on 29th Oct I installed tati-micheletti/reproducible 
# ref = daccc226839b2b7970e6af6721126f91bd806dce (Which includes my fix to preProcess / guessAtFile)

# HAVE TO RE-WRITE THE CODE TO INCLUDE IN THE FINAL MERGED FILES THE NAME OF THE focal distance!!!

# FOR 500m
# DOUBLE CHECK IF FOCAL WEIGHT WILL GET THE CACHE. IT SHOULD NOT!

library(SpaDES.core)
library(SpaDES.tools)
tryCatch(library(unixtools),
         error = function(e) install.packages("unixtools", repos = 'http://www.rforge.net/'))
options("reproducible.useMemoise" = FALSE) # Avoids bringing cache to memory
unixtools::set.tempdir("/mnt/storage/temp")


# set the directories
workDirectory <- getwd()

paths <- list(
  # As the project takes up a LOT of space, all mid steps will be saved inside the cache folder of another partition,
  cachePath = file.path("/mnt/storage/borealBirdsAndForestry", "cache"),
  # while the other folders are in the working directory
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

options("reproducible.cachePath" = paths$cachePath)
setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)

## list the modules to use
modules <- list("birdDensityBCR_Prov_LCC", "loadOffsetsBAM", "glmerBirdModels", "prepTiles", "focalCalculation")
#Complete set of modules: "birdDensityBCR_Prov_LCC", "loadOffsetsBAM", "glmerBirdModels", "prepTiles", 
                         #"focalCalculation", "predictBirds", "birdAbundanceTrends", "finalRasterPlots

## Set simulation and module parameters
times <- list(start = 1985, end = 2011, timeunit = "year") # Change back to 1985 2011. Just did 3-10 because of fake Rasters
parameters <- list(
  birdDensityBCR_Prov_LCC = list(extractFrom4kRasters = FALSE,
                                 avoidAlbertosData = TRUE,
                                 testArea = TRUE),
  bayesianBirdModel = list(testArea = TRUE), # FALSE means using boral shapefile to crop and mask
  glmerBirdModels = list(cropForModel = FALSE,
                         avoidAlbertosData = TRUE),
  prepTiles = list(testArea = TRUE, # Should a study area be used (i.e. boreal)?
                        nx = 12, # mult 7
                        ny = 5, # mult 3
                        rType = "INT1U",
                        buffer = c(1300,1300), # Buffer to make sure that when rasters are slip, they won't have edge effects
                        .useCache = FALSE), # Should it override module's .useCache?
  focalCalculation = list(recoverTime = 30,
                           resampledRes = 250,
                           focalDistance = 100, # To run for neighborhood, change to c(100, 500)
                           disturbanceClass = 2, # 2 = Forestry, 1 = Fire, 3 and 4 = low probability forestry and fire
                           forestClass = 1:6, # Forested area class in the land cover map. If changing to fire might need to be rethought. Or not...
                           useParallel = NULL) # "across" = across machines, "local" = only on local machine, "NULL" or anything else = no parallel
        )

objects <- list( # Possible to include 'rP' directly here as a shapefile!
  mapSubset = NULL, # "Canada" or Provinces to run at once. Good to subset provinces still within the boreal
  specificTestArea = "boreal", # "boreal",
  SQLtableVersion = "V4_2015", # Data retrieving from SQL: specific versions
  SQLServer = "boreal.biology.ualberta.ca", # Data retrieving from SQL: server
  SQLDatabase = "BAM_National_V4_2015_0206", # Data retrieving from SQL: specific database
  dataName = "Final_points_2010.csv", # Alberto's manuscript data to select points. Data are, however coming from SQL.
  birdSpecies = c("BBWA", # Bird species to run the models for
                  #"BLPW"#,
                  # "BOCH",
                  # "BRCR",
                  "BTNW",
                  "CAWA",
                  "CMWA"#,
                  # "CONW",
                  # "OVEN",
                  # "PISI",
                  # "RBNU",
                  # "SWTH",
                  # "TEWA",
                  # "WETA",
                  # "YRWA"
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
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
mySimOut <- spades(mySim, debug = TRUE)

# To save the outputs
# localAUG16 <- as(mySimOut, "simList_")
# saveRDS(localAUG16, file.path(outputPath(mySimOut), "localAUG16.rds"))

# To load the outputs
# mySimOut <- readRDS(file.path(outputPath(mySimOut), "backcast_10July2018.rds"))

