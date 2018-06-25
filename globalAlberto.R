# Global script for the glmerBirdModels

#devtools::install(local = FALSE)
#try(detach("package:SpaDES.core", unload = TRUE)); devtools::load_all("~/Documents/GitHub/SpaDES.tools"); devtools::load_all("~/Documents/GitHub/SpaDES.core")

#Note: BRCR for localPermanent and for localUndisturbedBoth didn't converge even taking the interactive random effect out, but it did taking the year (random effect) as well.
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
modules <- list("glmerBirdModels") # #bayesianBirdModel

## Set simulation and module parameters
times <- list(start = 1985, end = 1985, timeunit = "year")
parameters <- list(
    glmerBirdModels = list(cropping = TRUE, cropForModel = FALSE)
)

objects <- list(
    dataName = "Final_points_2010.csv",
    birdSpecies = c(
                     "BBWA",
                     "BLPW",
                     "BOCH",
                     "BRCR"
                     ,
                     "BTNW", "CAWA",
                     "CMWA","CONW",
                     "OVEN", "PISI",
                     "RBNU", "SWTH",
                     "TEWA", "WETA",
                     "YRWA"
                    ),
    typeDisturbance = c("Permanent", "Transitional", "Both"), # 
    disturbanceDimension = c("local", "neighborhood", "LocalUndisturbed") #
)

## Using external viewer
# dev.useRSGD(FALSE) # do not use Rstudio graphics device
# dev() # opens external (non-RStudio) device, which is faster

## Simulation setup
mySim <- simInit(times = times, params = parameters, modules = modules, paths =  paths, objects = objects)
system.time(mySimOut <- spades(mySim, debug = TRUE))

# To save the outputs
mySimList <- as(mySimOut, "simList_")
saveRDS(mySimList, file.path(outputPath(mySimOut), "Results21JUN18.rds"))

# To load the outputs
# mySimOut <- readRDS(file.path(outputPath(mySimOut), "Results19JUN18.rds"))
