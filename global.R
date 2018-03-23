# Global script for the glmerBirdModels

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
modules <- list("glmerBirdModels")

## Set simulation and module parameters
times <- list(start = 1985, end = 1985, timeunit = "year")
parameters <- list(
  .globals = list(.plotInitialTime = 1),
  glmerBirdModels = list(cropping = TRUE, cropForModel = FALSE)
)

objects = list(
  birdSpecies = c("BBWA", "BLPW", "BOCH", "BRCR",
                               "BTNW", "CAWA", "CMWA","CONW",
                               "OVEN", "PISI", "RBNU", "SWTH",
                               "TEWA", "WETA", "YRWA"),
               typeDisturbance = c("Transitional", "Permanent","Both"),
               disturbanceDimension = c("local", "neighborhood", "LocalUndisturbed"),
               dataName = "Final_points_BEAD_final.csv")

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
system.time(mySimOut <- spades(mySim, debug = TRUE))

# To save the outputs
mySimList <- as(mySimOut, "simList_")
saveRDS(mySimList, file.path(outputPath(mySimOut), "Results_List.rds"))
saveRDS(mySimOut, file.path(outputPath(mySimOut), "Results_Env.rds"))

# To load the outputs
mySimOut <- readRDS(file.path(paths$outputPath, "Results.rds"))

# To see the objects that are interacting in this module:
objectDiagram(mySim)
