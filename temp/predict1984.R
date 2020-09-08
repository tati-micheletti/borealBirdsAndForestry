
# Testing for install of GDAL / These scripts are temporarily in the 'inputs' folder. Should be pushed to 'pemisc' once it is working/passing again.
setwd("/home/tmichele/projects/borealBirdsAndForestry")
invisible(sapply(X = list.files(file.path(getwd(), "functions"), 
                                full.names = TRUE), FUN = source))
if (!isGDALInstalled()) message("GDAL was not found in your computer, please make sure you install it before running these modules.")
if (!length(Sys.which("unzip")) > 0) message("unzip was not found in your computer, please make sure you install it before running these modules.")

# We also need to set a 'scratch' temporary folder for raster (if our home folder doesn't have enough space)
maxMemory <- 5e+12
user <- pemisc::user()
scratchDir <- reproducible::checkPath(path = paste0("/mnt/tmp/rasterTMP/", user), create = TRUE)

#Here we check that the creation of the folder worked (we might have problems with writting access, only tested with my own user)
if(dir.create(scratchDir)) system(paste0("sudo chmod -R 777 /mnt/tmp/rasterTMP"), wait = TRUE) 
raster::rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, rasterTmpDir = scratchDir)

library("SpaDES.core")
library("SpaDES.tools")
reproducible::Require("ggplot2")
reproducible::Require("ggalt")

if (quickPlot::isRstudioServer()) options(httr_oob_default = TRUE)
workDirectory <- getwd()

# Set temp folder
tryCatch(library(unixtools),
         error = function(e) install.packages("unixtools", repos = 'http://www.rforge.net/'))
unixtools::set.tempdir(file.path(dirname(getwd()), "tmp"))

options('spades.moduleCodeChecks' = FALSE)
options('reproducible.useNewDigestAlgorithm' = FALSE)
options("reproducible.cachePath" = file.path(workDirectory, "cache"))
SpaDES.core::setPaths(modulePath = file.path(workDirectory, "modules"), 
                      inputPath = file.path(workDirectory, "modules/predictBirds/data/"), 
                      outputPath = file.path(workDirectory, "outputs"), 
                      cachePath = file.path(workDirectory, "cache"))

# Check for any log leftovers
leftoverLogs <- list.files(Paths$cachePath, pattern = "logParallel")
if (length(leftoverLogs) != 0)
  unlink(file.path(Paths$cachePath, leftoverLogs))

## list the modules to use
modules <- list("predictBirds")

## Set simulation and module parameters
times <- list(start = 1984, end = 1984, timeunit = "year")
parameters <- list(
  predictBirds = list(focalDistance = c(100, 500),
                      useParallel = FALSE,
                      savePredVectors = TRUE)
  )
tryCatch(googledrive::drive_download(file = googledrive::as_id("1S4ryXaqp0ZdW_yPBuYtNSPW5Mp1ft3R5"), 
                                     path = file.path(getPaths()$modulePath, "glmerBirdModels/data/models.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
tryCatch(googledrive::drive_download(file = googledrive::as_id("1KoL6QzKqCiBUZ8i6O-llCiit0G2APWLI"), 
                                     path = file.path(getPaths()$modulePath, "glmerBirdModels/data/data.rds")), 
         error = function(e){message("Files are already present and won't be overwritten")})
models <- readRDS(file = file.path(getPaths()$modulePath, "glmerBirdModels/data/models.rds"))
data <- readRDS(file = file.path(getPaths()$modulePath, "glmerBirdModels/data/data.rds"))
disturbancePredict <- "Transitional"
birdSpecies <- c("OVEN", "PISI", "RBNU", "SWTH", "TEWA", "WETA", "YRWA")
predictModels <- subsetModels(birdSp = birdSpecies, disturbancePredict = disturbancePredict, 
                              prmt = parameters, models = models)
# Get bird density rasters
fl <- unlist(lapply(X = birdSpecies, FUN = function(bird){
  fl <- usefun::grepMulti(x = list.files(path = file.path(getwd(), "modules/birdDensityBCR_Prov_LCC/data"), 
                                         full.names = TRUE), patterns = c("density", bird, ".tif"))
}))

birdDensityRasters <- lapply(X = fl, FUN = function(rasPath){
  bird <- usefun::substrBoth(strng = tools::file_path_sans_ext(rasPath), 
                             howManyCharacters = 4, fromEnd = TRUE)
  birdRas <- raster::raster(rasPath)
  return(birdRas)
})
names(birdDensityRasters) <- usefun::substrBoth(strng = tools::file_path_sans_ext(fl), 
                                                howManyCharacters = 4, fromEnd = TRUE)

.objects <- list( # Possible to include 'rP' directly here as a shapefile!
  predictModels = predictModels,
  focalDistance = 500,
  birdDensityRasters = birdDensityRasters, 
  data = data,
  birdSpecies = birdSpecies,
  disturbancePredict = disturbancePredict #, # Needs to match disturbanceClass from prediction module. Type of disturbance we want to predict from.
)

clearPlot()

## Simulation setup
prediction1984_500 <- SpaDES.core::simInitAndSpades(times = times, params = parameters, 
                                                  modules = modules, paths =  Paths,
                                                  objects = .objects, debug = 1)
saveRDS(prediction1984_500, file = file.path(getwd(), "outputs/08JUL2019_nonSignificant/prediction1984_500_rest.rds"))

