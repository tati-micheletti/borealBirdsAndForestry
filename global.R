# Global script for birdForestry project

library(SpaDES)

# set the directories
workDirectory <- getwd()

paths <- list(
  cachePath = file.path(workDirectory, "cache"),
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)

## list the modules to use
modules <- list("glmerBirdModels") #birdDensityBCR_Prov_LCC
#modules <- list("birdDensityBCR_Prov_LCC") #

## Set simulation and module parameters

times <- list(start = 1985, end = 2000, timeunit = "year")
parameters <- list(
 .globals = list(.plotInitialTime = 1),
 glmerBirdModels = list(cropping = TRUE, cropForModel = FALSE, start = 1985, end = 1985)
  #,birdDensityBCR_Prov_LCC = list(start = 1985, end = 1985)
)

objects = list(#studyAreaName = "testArea.shp",
               birdSpecies = c("BBWA", "BLPW", "BOCH", "BRCR",
                                "BTNW", "CAWA", "CMWA",
                                "OVEN", "PISI", "RBNU", "SWTH",
                                "TEWA", "WETA", "YRWA"),
               #  birdSpecies = "CONW",
               #birdSpecies = c("BBWA", "BLPW"),
               typeDisturbance = c("Transitional", "Permanent", "Both"),
               disturbanceDimension = c("local", "neighborhood", "LocalUndisturbed"),
               dataName = "Final_points_BEAD_final.csv")

# Problematic birds: "CONW"

#objects = list(birdSpecies = c("PISI", "BLPW"))

# dev.useRSGD(TRUE) # do not use Rstudio graphics device
# dev() # opens external (non-RStudio) device, which is faster
# clearPlot()

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
mySimOut <- spades(mySim, debug = TRUE) #c("warblersPointCountBC","init")

moduleDiagram(mySim)
objectDiagram(mySim)

