library(SpaDES)
library(rgdal)
library(raster)

#assumee you have cache inputs modules and outputs in borealBirdsAndForestry folder
#Due to splitRaster, this will write rasters to data but you can delete them after
setPaths(cachePath = "cache", 
         inputPath = "inputs", 
         outputPath = "outputs",
         modulePath = "modules")
getPaths() # shows where the 4 relevant paths are

#Generate dummy data. This will be created by Tati's module in practice
# species <- c("Blue warbler", "Grinchy woodpecker")
species <- list("BBWA", "BLPW")

# x <- rnorm(124, mean = 10, sd = 2)
# x1 <- rnorm(124, mean = 6, sd = 1)
# y <- 40 - x - x1 + rnorm(124, mean = 6, sd = 1)
# mod1 <- glm(y~x, na.action = na.exclude)
# y2 <- 60 - x - 2*x1 + rnorm(124, mean = 20, sd = 0.5)
# mod2 <- glm(y2~x, na.action = na.exclude)
# models <- list("Bl.wa_mod" = mod1, "Gr.wo_mod" = mod2)

#Tati's models
mod <- readRDS("C:/Ian/Tati/AlbetoModels/birdModels.rda") #or assign this from a simOutput. Eventually it should link from sim
models <- mod$localTransitional[1:2]

#Generate dummy abundance data
Blwa_ab <- raster(ncol = 20, nrow = 20, xmn = -10, xmx = 10, ymn = -10, ymx = 10)
Blwa_ab <- setValues(Blwa_ab, values = rnorm(n= 400, mean = 30, sd = 4))
Grwo_ab <- raster(ncol = 20, nrow = 20, xmn = -10, xmx = 10, ymn = -10, ymx = 10)
Grwo_ab <- setValues(Grwo_ab, values = rnorm(n= 400, mean = 50, sd = 5))
abundances = list("Blwa_ab" = Blwa_ab, "Grwo_ab" = Grwo_ab)

# #generate dummy disturbance data
disturbanceType <- raster(ncol = 20, nrow = 20, xmn = -10, xmx = 10, ymn = -10, ymx = 10)
disturbanceType <- setValues(disturbanceType, round(runif(400, 1,6), digits = 0))
disturbanceYear <- raster(ncol = 20, nrow = 20, xmn = -10, xmx = 10, ymn = -10, ymx = 10)
disturbanceYear <- setValues(disturbanceYear, round(runif(400, 94, 107), digits = 0))
landcover <- raster(ncol = 20, nrow = 20, xmn = -10, xmx = 10, ymn = -10, ymx = 10)
landcover <- setValues(landcover, round(runif(400, 1, 12), digits = 0))

#Set up Spades Call
times <- list(start = 94, end = 107)
parameters <- list(splitModelPlot = list("focalDistance" = 3, 
                                         "disturbanceClass" = 2,
                                         "nx" = 2,
                                         "ny" = 2,
                                         "rType" = "FLT4S",
                                         "buffer" = c(3,3),
                                         "forestClass" = 1:6,
                                         ".useCache" = TRUE))
modules <- list("splitModelPlot")
objects <- list("disturbanceType" = disturbanceType,
                "disturbanceYear" = disturbanceYear,
                "landCover" = landcover,
                "inputSpecies" = species,
                "inputModels" = models,
                "inputAbundances" = abundances)
inputs <- list()
outputs <- list()

#options(spades.moduleCodeChecks = TRUE)
mySim <- simInit(times = times, params = parameters, modules = modules, objects = objects)

mySimOut <- spades(mySim, debug = TRUE)
