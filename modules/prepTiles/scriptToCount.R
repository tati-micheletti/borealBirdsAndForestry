
source(file.path(getwd(), 'functions/isGDALInstalled.R'))
if (!isGDALInstalled()) message("GDAL was not found in your computer, please make sure you install it before running these modules.")
if (!length(Sys.which("unzip")) > 0) message("unzip was not found in your computer, please make sure you install it before running these modules.")
# We also need to set a 'scratch' temporary folder for raster (if our home folder doesn't have enough space)
maxMemory <- 5e+12
user <- pemisc::user()
scratchDir <- reproducible::checkPath(path = file.path(dirname(getwd()), "scratch"), create = TRUE)

#Here we check that the creation of the folder worked (we might have problems with writting access, only tested with my own user)
if(dir.create(scratchDir)) system(paste0("sudo chmod -R 777 /mnt/tmp/rasterTMP"), wait = TRUE) 
raster::rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, rasterTmpDir = scratchDir)
# Make sure all packages are updated
updateGithubPackages <- FALSE

if (updateGithubPackages){
  devtools::install_github("PredictiveEcology/reproducible@development")
  devtools::install_github("achubaty/amc@development")
  devtools::install_github("PredictiveEcology/pemisc@development")
  devtools::install_github("PredictiveEcology/map@development")
  devtools::install_github("PredictiveEcology/LandR@development") # Updates SpaDES.tools and SpaDES.core quickPlot
  devtools::install_github("ianmseddy/LandR.CS@master") # Climate sensitivity in LandR
}

library("SpaDES.core")
library("SpaDES.tools")
reproducible::Require("ggplot2")
reproducible::Require("ggalt")
reproducible::Require("raster")

if (quickPlot::isRstudioServer()) options(httr_oob_default = TRUE)
workDirectory <- getwd()

Paths <- list(
  cachePath = file.path(workDirectory, "cache"),
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

# Set temp folder
tryCatch(library(unixtools),
         error = function(e) install.packages("unixtools", repos = 'http://www.rforge.net/'))
unixtools::set.tempdir(file.path(dirname(getwd()), "tmp"))

options('spades.moduleCodeChecks' = FALSE)
options('reproducible.useNewDigestAlgorithm' = FALSE)
options("reproducible.cachePath" = Paths$cachePath)
SpaDES.core::setPaths(modulePath = Paths$modulePath, 
                      inputPath = Paths$inputPath, 
                      outputPath = Paths$outputPath, 
                      cachePath = Paths$cachePath)

# Check for any log leftovers
leftoverLogs <- list.files(Paths$cachePath, pattern = "logParallel")
if (length(leftoverLogs) != 0)
  unlink(file.path(Paths$cachePath, leftoverLogs))

## list the modules to use
modules <- list("prepTiles")
times <- list(start = 2011, end = 2011, timeunit = "year")

parameters <- list(
  prepTiles = list(testArea = TRUE, # Should a study area be used (i.e. boreal)?
                   nx = 7, # mult 7
                   ny = 3, # mult 3
                   useParallel = NULL,
                   rType = "INT1U",
                   buffer = c(0, 0), # Buffer to make sure that when rasters are slip, they won't have edge effects
                   .useCache = FALSE) # Should it override module's .useCache?
)
posthocFolder <- file.path(getwd(),"outputs/posthocAnalysis")
modules <- list("prepTiles")
maskedDensityRasFolder <- file.path(posthocFolder, "maskedDensityRas")
RTM <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif"))
source(file.path(getwd(), 'functions/makeBCRandLCC.R'))
pathData <- file.path(getwd(), "modules/birdDensityBCR_Prov_LCC/data/")
BCRLCC05 <- Cache(makeBCRandLCC, 
                  pathData = pathData,
                  RTM = RTM,
                  userTags = c("objectName:BCRLCC05",
                               "script:paperPlots"), 
                  overwrite = TRUE, omitArgs = c("overwrite", 
                                                 "userTags", "useCache"))
managedForest <- Cache(prepInputs, url = "https://drive.google.com/open?id=1tgqn8FajD1iSj0aECONGhFzwInau0-q8",
                       targetFile = "NIR2016_MF.shp", archive = "NIR2016_MF.zip",
                       alsoExtract = "similar",
                       studyArea = BCRLCC05$BCR, rasterToMatch = BCRLCC05$LCC05, 
                       overwrite = TRUE, omitArgs = c("overwrite"),
                       destinationPath = file.path(getwd(), "inputs"), 
                       userTags = "objectName:managedForest")
objects <- list( # Possible to include 'rP' directly here as a shapefile!
  "rP" = managedForest
)

tiles <- simInitAndSpades(times = times, params = parameters, modules = modules,
                          objects = objects, debug = 2)
