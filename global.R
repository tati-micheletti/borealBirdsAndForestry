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
modules <- list("") # 

## Set simulation and module parameters

times <- list(start = 2009, end = 2020, timeunit = "year")
parameters <- list(
  .globals = list(.plotInitialTime = 1),
  warblersPointCountBC = list(overrideModel = TRUE, start = 2009, end = 2011)
)
objects = list(studyArea = "random", areaClass = "territory", areaName = "British Columbia", 
               polyMatrix = matrix(c(-122.85, 52.04), ncol = 2), areaSize = 500000,
               species = c("PISI","UEFL","YRWA","DEJU"))
# objects = list(studyArea = "random")

dev.useRSGD(FALSE) # do not use Rstudio graphics device
dev() # opens external (non-RStudio) device, which is faster
clearPlot()

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
mySimOut <- spades(mySim, debug = TRUE) #c("warblersPointCountBC","init")

moduleDiagram(mySim)
objectDiagram(mySim)