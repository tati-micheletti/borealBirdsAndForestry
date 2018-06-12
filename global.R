# Global script for the glmerBirdModels

#devtools::install(local = FALSE)
#try(detach("package:SpaDES.core", unload = TRUE)); devtools::load_all("~/Documents/GitHub/SpaDES.tools"); devtools::load_all("~/Documents/GitHub/SpaDES.core")
 library(SpaDES)

#debug(fixErrors)

# set the directories
 workDirectory <- getwd()

#workDirectory <- "/home/tmichele/Documents/GitHub/borealBirdsAndForestry" #For working on prepInputs
#debug(cropInputs)

paths <- list(
  cachePath = file.path(workDirectory, "cache"),
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)

## list the modules to use
modules <- list("birdDensityBCR_Prov_LCC", "glmerBirdModels", "splitModelPlot") # #bayesianBirdModel

## Set simulation and module parameters
times <- list(start = 1985, end = 1986, timeunit = "year")
parameters <- list(
    bayesianBirdModel = list(testArea = TRUE),
    glmerBirdModels = list(cropping = TRUE, cropForModel = FALSE),
    splitModelPlot = list(focalDistance = 3, 
                          disturbanceClass = 2,
                          nx = 2,
                          ny = 2,
                          rType = "FLT4S",
                          buffer = c(18,18),
                          forestClass = 1:6,
                          .useCache = TRUE)
)

objects <- list(
    dataName = "Final_points_BEAD_final.csv",
    birdSpecies = c("BBWA", 
                    # "BLPW", 
                    # "BOCH", "BRCR",
                    # "BTNW", "CAWA", 
                    # "CMWA","CONW",
                    # "OVEN", "PISI",
                    # "RBNU", "SWTH",
                    # "TEWA", "WETA", 
                    # "YRWA"
                    ),
    typeDisturbance = c("Transitional"), #, "Permanent", "Both"
    disturbanceDimension = c("local") #, "neighborhood", "LocalUndisturbed"
)

## Using external viewer
# dev.useRSGD(FALSE) # do not use Rstudio graphics device
# dev() # opens external (non-RStudio) device, which is faster

# clearPlot()

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
system.time(mySimOut <- spades(mySim, debug = TRUE))

# To save the outputs
# mySimList <- as(mySimOut, "simList_")
# saveRDS(mySimList, file.path(outputPath(mySimOut), "bayesResults.rds"))

# To load the outputs
# mySimOut <- readRDS(file.path(outputPath(mySimOut), "Results_List.rds"))

# To see the objects that are interacting in this module:
# objectDiagram(mySim)
