# 1. BCR_Prov_LCC means in a specific region:
library("reproducible")
library("usefun")
library("magrittr")
library("SpaDES")
options(reproducible.useCache = TRUE)

BCR8_ON <- usefun::provinceBCRStudyArea(bcr = 8, province = "Ontario", country = "CAN", useCache = TRUE)
BCR8_MT <- usefun::provinceBCRStudyArea(bcr = 8, province = "Manitoba", country = "CAN", useCache = TRUE)

bird <- "RBNU"
setPaths(cachePath = file.path("/mnt/data/Micheletti/borealBirdsAndForestry/cache/"),
  modulePath = file.path("/mnt/data/Micheletti/borealBirdsAndForestry/modules/"),
  inputPath = file.path("/mnt/data/Micheletti/borealBirdsAndForestry/outputs/trends500/"),
  outputPath = file.path("/mnt/data/Micheletti/borealBirdsAndForestry/outputs/posthocAnalysis/")
)

BCR_Prov_LCC_density <- simInitAndSpades(times = list(start = 0, end = 0), 
                 params = list(birdDensityBCR_Prov_LCC = list(extractFrom4kRasters = FALSE,
                                                                avoidAlbertosData = TRUE,
                                                                testArea = TRUE)),
                 modules = list("birdDensityBCR_Prov_LCC"),
                 objects = list("rP" = BCR8_ON,
                                "birdSpecies" = bird))

birdDensityOriginal <- raster::raster(BCR_Prov_LCC_density$birdDensityRasters[[bird]])
library(raster)
source('/mnt/data/Micheletti/borealBirdsAndForestry/modules/habitatDensityMasking/R/createBirdsRangeRasters.R')
source('/mnt/data/Micheletti/borealBirdsAndForestry/modules/habitatDensityMasking/R/getSpRange.R')
birdsRangeList <- createBirdsRangeRasters(birdSpecies = bird)
rangeRBNU <- getSpRange(species = bird, birdsRangeList = birdsRangeList)
slopeDensity <- raster::raster("/mnt/data/Micheletti/borealBirdsAndForestry/outputs/trends500/mergedTrendRBNU500.tif")
density1985 <- raster::raster("/mnt/data/Micheletti/borealBirdsAndForestry/modules/predictBirds/data/predictedRBNU500mYear1985.tif")
density2011 <- raster::raster("/mnt/data/Micheletti/borealBirdsAndForestry/modules/predictBirds/data/predictedRBNU500mYear2011.tif")


source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/calculateMeanBirdDensity.R')
# ONTARIO ~ Not significantly different
expectedAbund_ON <- Cache(calculateMeanBirdDensity, shape = BCR8_ON, ras = birdDensityOriginal, type = "Expected",  userTags = c("original", "abundEstimation"))
Abund1985_ON <- Cache(calculateMeanBirdDensity, shape = BCR8_ON, ras = density1985, type = "1985",  userTags = c("1985", "abundEstimation"))
Abund2011_ON <- Cache(calculateMeanBirdDensity, shape = BCR8_ON, ras = density2011, type = "2011",  userTags = c("2011", "abundEstimation"))
abundTable_ON <- rbind(expectedAbund_ON$table, Abund1985_ON$table, Abund2011_ON$table)

# MANITOBA
expectedAbund_MT <- Cache(calculateMeanBirdDensity, shape = BCR8_MT, ras = birdDensityOriginal, type = "Expected",  userTags = c("original", "abundEstimation"))
Abund1985_MT <- Cache(calculateMeanBirdDensity, shape = BCR8_MT, ras = density1985, type = "1985",  userTags = c("1985", "abundEstimation"))
Abund2011_MT <- Cache(calculateMeanBirdDensity, shape = BCR8_MT, ras = density2011, type = "2011",  userTags = c("2011", "abundEstimation"))
abundTable_MT <- rbind(expectedAbund_MT$table,Abund1985_MT$table, Abund2011_MT$table)

# --------------------------
pixelsToCheck <- usefun::subsetNonNARas(ras = birdDensityOriginal, 100)
subsetFromOriginal <- birdDensityOriginal[pixelsToCheck]
summRBNUMod <- summary(models$neighborhoodTransitional$RBNU)
logDensityEstimate <- summRBNUMod$coefficients["logDENSITY_RBNU", "Estimate"]
interceptEstimate <- summRBNUMod$coefficients["(Intercept)", "Estimate"]
# expDistZeroFromAlbMod <- logDensityEstimate*log(subsetFromOriginal)

subsetSlope <- slopeDensity[pixelsToCheck]
subsetFrom1985 <- density1985[pixelsToCheck]
subsetPre1985 <- subsetSlope*subsetFrom1985

# NOW: 
# 1. Same, but for the slope "pre-1984" meaning if YEAR == 0? Think, 1984 and 2011
# 2. Same, but for the 30m focal "pre-1984" meaning if YEAR == 0? Think, 1984 and 2011

# TESTING WHY THE PREDICTIONS ARE SO OFF
# ORIGINAL DENSITY
rasDensity <- reproducible::postProcess(x = raster::raster("/mnt/data/Micheletti/borealBirdsAndForestry/modules/birdDensityBCR_Prov_LCC/data/densityRBNU.tif"), 
                                        studyArea = BCR8_MT, filename2 = NULL)

birdDensityVectors <- raster::getValues(rasDensity)

# ORIGINAL DISTURBANCE
disturbanceRas <- reproducible::postProcess(x = raster::raster("/mnt/data/Micheletti/borealBirdsAndForestry/modules/focalCalculation/data/mergedFocal1985-500Res250m.tif"), 
                                        studyArea = BCR8_MT, rasterToMatch = rasDensity, filename2 = NULL)

disturbanceVec <- raster::getValues(disturbanceRas)

models <- readRDS(file = file.path(getPaths()$modulePath, "glmerBirdModels/data/models.rds"))
modelRBNU <- models$neighborhoodTransitional$RBNU

LOGbirdDensityVectors <- log(birdDensityVectors) # log the value of densities so it is the same of the original model
birdDensityVectors[birdDensityVectors < -0.99] <- -1 # Why did I do this? Maybe because we were not supposed to have values smaller than -0.99?
vecDF <- data.frame(disturbanceVec, LOGbirdDensityVectors)
colnames(vecDF) <- c("State_P_500", "logDENSITY_RBNU") # CHANGE FOR THE MODEL'S NAME
source('/mnt/data/Micheletti/borealBirdsAndForestry/modules/predictBirds/R/fitModel.R')
predicted <- suppressWarnings(fitModel(inRas = vecDF, 
                                       inputModel = modelRBNU, 
                                       x = "RBNU", 
                                       tileYear = 1985))
predRas <- raster::setValues(x = raster(disturbanceRas), values = as.numeric(predicted))

# DENSITY I HAVE
previouslyPredDensity <- reproducible::postProcess(x = raster::raster("/mnt/data/Micheletti/borealBirdsAndForestry/modules/predictBirds/data/predictedRBNU500mYear1985.tif"),
                                                   studyArea = BCR8_MT, filename2 = NULL)
names(previouslyPredDensity) <- "predictedBefore"
predRas <- reproducible::postProcess(x = predRas,
                                     studyArea = BCR8_MT, 
                                     rasterToMatch = previouslyPredDensity, 
                                     filename2 = NULL)
names(predRas) <- "predictedNow"

parSetup <- par()
invisible(on.exit(par(parSetup)))
par(mfrow=c(1,2))
  
plot(previouslyPredDensity)
plot(predRas)

hist(previouslyPredDensity)
hist(predRas)

# Try fitting a glmer model without the RE to see if this is the difference that I am observing in the model
tryCatch(googledrive::drive_download(file = googledrive::as_id("1KoL6QzKqCiBUZ8i6O-llCiit0G2APWLI"), 
                                     path = file.path(getPaths()$modulePath, "glmerBirdModels/data/data.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
data <- readRDS(file = file.path(getPaths()$modulePath, "glmerBirdModels/data/data.rds"))

dt <- data$neighborhoodTransitional

source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/plotModelChecking.R')
birdSpecies <- c("BBWA", # Bird species to run the models for
                 "BLPW",
                 "BOCH",
                "BRCR",
                 "BTNW",
                 "CAWA",
                 "CMWA",
                 "CONW",
                 "OVEN",
                 "PISI",
                 "RBNU",
                 "SWTH",
                 "TEWA",
                 "WETA",
                 "YRWA"
)
plotBirds <- lapply(X = birdSpecies, FUN = function(bird){
  pl <- plotModelChecking(species = bird, dt = dt, 
                          disturbancePercentage = 0)
})
names(plotBirds) <- birdSpecies
plotBirds

plotBirdsDist <- lapply(X = birdSpecies, FUN = function(bird){
  dist <- lapply(c(0.1, 0.5, 1), function(d){  
    pl <- plotModelChecking(species = bird, dt = dt,
                            disturbancePercentage = d)
  })
})
names(plotBirdsDist) <- birdSpecies
# pl <- plotModelChecking(species = "BBWA", dt = dt, 
#                         disturbancePercentage = 0)
