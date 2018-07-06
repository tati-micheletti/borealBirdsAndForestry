# Global script for the glmerBirdModels

#devtools::install(local = FALSE)
library(SpaDES.core)
library(SpaDES.tools)

#debug(fixErrors)

# set the directories
 workDirectory <- getwd()

#workDirectory <- "/home/tmichele/Documents/GitHub/borealBirdsAndForestry" #For working on prepInputs
#debug(cropInputs)

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
modules <- list("birdDensityBCR_Prov_LCC", "glmerBirdModels", "splitModelPlot") # #bayesianBirdModel

## Set simulation and module parameters
times <- list(start = 1985, end = 1987, timeunit = "year")
parameters <- list(
    bayesianBirdModel = list(testArea = TRUE),
    glmerBirdModels = list(cropping = TRUE, 
                           cropForModel = FALSE),
    splitModelPlot = list(testArea = TRUE,
                          focalDistance = 100, # To run for neighborhood, change to 500
                          disturbanceClass = 2, # 2 = Forestry, 1 = Fire, 3 and 4 = low probability forestry and fire
                          nx = 3,
                          ny = 2,
                          rType = "INT1U",
                          buffer = c(18,18),
                          forestClass = 1:6,
                          .useCache = TRUE)
)

objects <- list(
    dataName = "Final_points_2010.csv",
    birdSpecies = c("BBWA",
                     "BLPW", 
                    # "BOCH", "BRCR",
                     # "BTNW", "CAWA", 
                    # "CMWA","CONW",
                    # "OVEN", "PISI",
                    # "RBNU", "SWTH",
                    # "TEWA", "WETA", 
                     "YRWA"
                    ),
    typeDisturbance = c("Transitional", "Permanent"), #, "Permanent", "Both"
    disturbanceDimension = c("local", "neighborhood"), #, "neighborhood", "LocalUndisturbed"
    disturbancePredict = c("Transitional") # Needs to match disturbanceClass from prediction module
)

## Using external viewer
# dev.useRSGD(FALSE) # do not use sRstudio graphics device
# dev() # opens external (non-RStudio) device, which is faster

# clearPlot()

# debug(postProcess)
## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
mySimOut <- spades(mySim, debug = TRUE)

# To save the outputs
# mySimList <- as(mySimOut, "simList_")
# saveRDS(mySimList, file.path(outputPath(mySimOut), "bayesResults.rds"))

# To load the outputs
# mySimOut <- readRDS(file.path(outputPath(mySimOut), "Results_List.rds"))

# To see the objects that are interacting in this module:
# objectDiagram(mySim)
