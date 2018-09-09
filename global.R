# Global script for the Backcasting Project

# STILL NEED TO CHANGE testArea = FALSE to use boreal shapefile to crop and mask to
# CAWA: Canada Warbler
# BBWA: Bay-breasted Warbler
# CMWA: Cape May Warbler
# BTNW: Black-throated Green Warbler

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

setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)

# if (dir.exists("/mnt/storage/borealBirdsAndForestry/cache/outputRasters")){ # Delete all previous tiles so we have only the most updated ones
#   unlink(file.path(paths$cachePath, "outputRasters"), recursive = TRUE)
# }

area1 <- c("Yukon", "British Columbia")
area2 <- c("Alberta", "Saskatchewan")
area3 <- "Northwest Territories"
area4 <- c("Manitoba", "Nunavut")
area5 <- "Ontario"
area6 <- "Quebec"
area7 <- c("New Brunswick", "Newfoundland and Labrador", "Nova Scotia", "Prince Edward Island")

## list the modules to use
modules <- list("birdDensityBCR_Prov_LCC", "loadOffsetsBAM", "glmerBirdModels", "splitModelPlot")# "finalRasterPlots" #"bayesianBirdModel"

## Set simulation and module parameters
times <- list(start = 1985, end = 2011, timeunit = "year")
parameters <- list(
  birdDensityBCR_Prov_LCC = list(extractFrom4kRasters = FALSE,
                                 avoidAlbertosData = TRUE,
                                 testArea = TRUE),
  bayesianBirdModel = list(testArea = TRUE), # FALSE means using boral shapefile to crop and mask
  glmerBirdModels = list(cropForModel = FALSE,
                         avoidAlbertosData = TRUE),
  splitModelPlot = list(extractFrom4kRasters = FALSE,
                        recoverTime = 30,
                        testArea = TRUE,
                        focalDistance = 100, # To run for neighborhood, change to 500
                        disturbanceClass = 2, # 2 = Forestry, 1 = Fire, 3 and 4 = low probability forestry and fire
                        nx = 21, # mult 7
                        ny = 9, # mult 3
                        rType = "INT1U",
                        buffer = c(18,18),
                        forestClass = 1:6,
                        .useCache = FALSE,
                        useParallel = NULL), # "across" = across machines, "local" = only on local machine, "NULL" or anything else = no parallel
  # As of 12th July, 'across' is not working... See notes above
  finalRasterPlots = list(testArea = TRUE)
)

objects <- list( # Possible to include 'rP' directly here as a shapefile!
  mapSubset = area3, # Provinces to run at once 
  specificTestArea = "boreal",
  SQLtableVersion = "V4_2015",
  SQLServer = "boreal.biology.ualberta.ca",
  SQLDatabase = "BAM_National_V4_2015_0206",
  dataName = "Final_points_2010.csv", #Manuscript file was Final_points_2010.csv; testing to compare with Final_points_2010_updatedDensity.csv
  birdSpecies = c("BBWA"#,
                  #"BLPW"#,
                  # "BOCH",
                  # "BRCR",
                  # "BTNW",#,
                  # "CAWA",
                  # "CMWA"#,
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
  disturbancePredict = c("Transitional") # Needs to match disturbanceClass from prediction module
)

clearPlot()

# file.remove("/mnt/storage/borealBirdsAndForestry/cache/logParallel")

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
system.time(mySimOut <- spades(mySim, debug = TRUE))

# To save the outputs
 localAUG16 <- as(mySimOut, "simList_")
 saveRDS(localAUG16, file.path(outputPath(mySimOut), "localAUG16.rds"))

# To load the outputs
# mySimOut <- readRDS(file.path(outputPath(mySimOut), "backcast_10July2018.rds"))

