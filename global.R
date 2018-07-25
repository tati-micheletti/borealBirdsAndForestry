# Global script for the Backcasting Project

# Bird density data: https://borealbirds.databasin.org/galleries/143b56bbc7584bd7a44ba86119061b15

# STILL NEED TO CHANGE testArea = FALSE to use boreal shapefile to crop and mask to
# CAWA: Canada Warbler
# BBWA: Bay-breasted Warbler
# CMWA: Cape May Warbler
# BTNW: Black-throated Green Warbler

# IF NEED TO WORK ON PREPINPUTS
# try(detach("package:SpaDES.addins", unload = TRUE)); 
# try(detach("package:SpaDES.core", unload = TRUE)); 
# try(detach("package:SpaDES.tools", unload = TRUE)); 
# try(detach("package:reproducible", unload = TRUE)); 
# devtools::load_all("~/Documents/GitHub/reproducible"); 
# # library(SpaDES.core)
## devtools::load_all("~/Documents/GitHub/SpaDES.core")
# library(SpaDES.tools)
##devtools::load_all("~/Documents/GitHub/SpaDES.tools"); 

library(SpaDES.core)
library(SpaDES.tools)

# set the directories
 # workDirectory <- getwd()
workDirectory <- "/home/tmichele/Documents/GitHub/borealBirdsAndForestry"

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

## list the modules to use
modules <- list("loadOffsetsBAM", "birdDensityBCR_Prov_LCC", "glmerBirdModels") #  #bayesianBirdModel, #,  "splitModelPlot"

## Set simulation and module parameters
times <- list(start = 1985, end = 2011, timeunit = "year")
parameters <- list(
    birdDensityBCR_Prov_LCC = list(extractFrom4kRasters = FALSE,
                                   testArea = TRUE),
    bayesianBirdModel = list(testArea = TRUE), # FALSE means using boral shapefile to crop and mask
    glmerBirdModels = list(cropForModel = FALSE),
    splitModelPlot = list(recoverTime = 30,
                          testArea = TRUE,
                          focalDistance = 100, # To run for neighborhood, change to 500
                          disturbanceClass = 2, # 2 = Forestry, 1 = Fire, 3 and 4 = low probability forestry and fire
                          nx = 3, # mult 7
                          ny = 4, # mult 3
                          rType = "INT1U",
                          buffer = c(18,18),
                          forestClass = 1:6,
                          .useCache = FALSE,
                          useParallel = "across") # "across" = across machines, "local" = only on local machine, "NULL" or anything else = no parallel
    # As of 12th July, 'across' is not working... See notes on ToDo.R
)

objects <- list(
    SQLtableVersion = "V4_2015",
    SQLServer = "boreal.biology.ualberta.ca",
    SQLDatabase = "BAM_National_V4_2015_0206",
    densityEstimatesURL = "https://drive.google.com/open?id=1Gd9VsKIXX_384UxQsJnwzsSZFuyQyscw",
    densityEstimatesFileName = "Habitat_Association_by_jurisdiction(bamddb_Apr19-2012).csv",
    newestDataset = "offsets-v3-new-chunk_2017-05-26.Rdata",
    olderDataset = "offsets-v3_2017-04-19.Rdata", # If the file only has one dataset, especify it as newestDataset and NULL olderDataset
    dataName = "Final_points_2010.csv", #Manuscript file was Final_points_2010.csv; test to compare with Final_points_2010_updatedDensity.csv
    birdSpecies = c("BBWA",
                    # "BLPW", 
                    # "BOCH", 
                    # "BRCR",
                    # "BTNW", 
                    # "CAWA", 
                    # "CMWA",
                    # "CONW",
                    # "OVEN", 
                    # "PISI",
                    # "RBNU", 
                    # "SWTH",
                    # "TEWA",
                    # "WETA", 
                    "YRWA"
                    ),
    typeDisturbance = c("Transitional", "Permanent"), #, "Permanent", "Both"
    disturbanceDimension = c("local", "neighborhood"), #, "neighborhood", "LocalUndisturbed"
    disturbancePredict = c("Transitional") # Needs to match disturbanceClass from prediction module
)

clearPlot()

# file.remove("/mnt/storage/borealBirdsAndForestry/cache/logParallel")

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
mySimOut <- spades(mySim, debug = TRUE)

# To save the outputs
# mySimList <- as(mySimOut, "simList_")
# saveRDS(mySimList, file.path(outputPath(mySimOut), "backcast_10July2018.rds"))

# To load the outputs
# mySimOut <- readRDS(file.path(outputPath(mySimOut), "backcast_10July2018.rds"))

