############################################
############################################
#    I n  i t i a l     S e t u p          #  
############################################
############################################

# Authorize GDrive
if (!exists("usrEmail"))
  usrEmail <- if (pemisc::user() %in% c("tmichele", "Tati")) "tati.micheletti@gmail.com" else 
    NULL
googledrive::drive_auth(email = usrEmail)
# googledrive::drive_auth(email = usrEmail, use_oob = TRUE) # FIRST TIME!

# Update Packages and Modules
if (all(pemisc::user() %in% c("Tati", "tmichele"), 
        getwd() != "/home/tmichele/projects/borealBirdsAndForestry"))
  message(crayon::red(paste0("Your current working directory is ", getwd(), 
                             ". Please make sure it is correct!"), 
          immediate. = TRUE))
  
if (!exists("updateCRAN")) updateCRAN <- FALSE
if (!exists("updateGithubPackages")) updateGithubPackages <- FALSE
if (!exists("updateSubmodules")) updateSubmodules <- FALSE

if (updateCRAN)
  update.packages(checkBuilt = TRUE, ask = FALSE)

if (updateGithubPackages){
  if (pemisc::user() %in% c("emcintir", "tmichele")) Sys.setenv("R_REMOTES_UPGRADE"="never")
  Pkg <- c("PredictiveEcology/Require@development",
           "PredictiveEcology/reproducible@development",
           "PredictiveEcology/quickPlot@development",
           "PredictiveEcology/SpaDES.addins@development",
           "PredictiveEcology/SpaDES.tools@development",
           "PredictiveEcology/SpaDES.core@development",
           "PredictiveEcology/pemisc@development",
           "achubaty/amc@development",
           "PredictiveEcology/map@development",
           "PredictiveEcology/LandR@master",
           "PredictiveEcology/usefulFuns@development",
           "ianmseddy/LandR.CS@master",
           "PredictiveEcology/fireSenseUtils@iterative",
           "PredictiveEcology/SpaDES@development")
  pkg <- lapply(Pkg, function(p){
    capture.output(devtools::install_github(p))
  })
  
  if (sum(sapply(pkg, length)) != 0){
    message(crayon::bgWhite(paste0("At least one new package was installed. ",
                                   "Restarting R. Please re-run your code")))
    .rs.restartR()  
  } else {
    message(crayon::green(paste0("No new packages were installed. ",
                                 "Your setup will continue.")))
  }
}


if (updateSubmodules){
  system(paste0("cd ", getwd(),
                " && git submodule foreach git pull"), wait = TRUE)
  system(paste0("cd ", getwd(),
                " && git pull"), wait = TRUE)
  system("git submodule", wait = TRUE) # checks if the branches and commits you are using are the correct ones
} 

library("usefulFuns")
library("data.table")
library("LandR")
library("raster")
library("plyr"); library("dplyr")
library("amc")
library("magrittr") # for piping
library("future")
library("future.apply")
library("crayon")
library("reproducible")
library("SpaDES.core")

# Source all common functions
invisible(sapply(X = c(file.path(getwd(), "/functions/isGDALInstalled.R")), FUN = source))
if (!isGDALInstalled()) message(paste0(crayon::red("GDAL was not found in your computer, ",
                                                   "please make sure you install it before ",
                                                   "running these modules.")))
if (!length(Sys.which("unzip")) > 0) message(paste0(crayon::red("unzip was not found in your computer, ",
                                                                "please make sure you install it before ",
                                                                "running these modules.")))
# Setting paths
outputSubFolder <- paste0("outputs/", toupper(format(Sys.time(), "%d%b%y")))
message(crayon::underline(crayon::yellow(paste0("Your outputs folder is: ", 
                                                crayon::green(file.path(getwd(), outputSubFolder))))))

message("Your current temporary directory is ", tempdir())
maxMemory <- 5e+12
scratchDir <- file.path("~/scratch/borealForestryAndBirds")
if(dir.create(scratchDir)) system(paste0("chmod -R 777 ", scratchDir), wait = TRUE)
raster::rasterOptions(default = TRUE)
options(rasterMaxMemory = maxMemory, rasterTmpDir = scratchDir)

SpaDES.core::setPaths(modulePath = file.path(getwd(), "modules"), 
                      inputPath = file.path(getwd(), "inputs"), 
                      outputPath = file.path(getwd(), outputSubFolder), 
                      cachePath = file.path(getwd(), "cache"), 
                      rasterPath = scratchDir)

# Check for any log leftovers
leftoverLogs <- list.files(SpaDES.core::Paths$cachePath, pattern = "logParallel")
if (length(leftoverLogs) != 0)
  unlink(file.path(Paths$cachePath, leftoverLogs))

# Sessting up the cache folder: it is hosted in the project's GDrive
cloudFolderID <- "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0"

# Setting all SpaDES options to be used in the project
.plotInitialTime <- NA
opts <- options(
  "spades.recoveryMode" = 2,
  "LandR.assertions" = FALSE,
  "LandR.verbose" = 1,
  "map.dataPath" = SpaDES.core::Paths$inputPath,
  "map.overwrite" = TRUE,
  "map.useParallel" = TRUE, #!identical("windows", .Platform$OS.type),
  "reproducible.futurePlan" = FALSE,
  "future.globals.maxSize" = if (pemisc::user("tmichele")) 6000*1024^2 else 6000*1024^2,
  "reproducible.cacheSaveFormat" = "qs",
  "reproducible.qsPreset" = "fast",
  "reproducible.inputPaths" = if (pemisc::user("emcintir")) "~/data" else SpaDES.core::Paths$inputPath,
  "reproducible.quick" = FALSE,
  "reproducible.overwrite" = TRUE,
  "reproducible.useMemoise" = TRUE, # Brings cached stuff to memory during the second run
  "reproducible.useNewDigestAlgorithm" = TRUE,  # use the new less strict hashing algo
  "reproducible.useCache" = TRUE,
  "reproducible.cachePath" = SpaDES.core::Paths$cachePath,
  "reproducible.showSimilar" = TRUE,
  "reproducible.useCloud" = FALSE,
  "spades.moduleCodeChecks" = FALSE, # Turn off all module's code checking
  "spades.useRequire" = TRUE, # assuming all pkgs installed correctly
  "pemisc.useParallel" = TRUE
)
