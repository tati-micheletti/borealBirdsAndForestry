
library(SpaDES.core)
library(SpaDES.tools)

# set the directories
workDirectory <- getwd()

paths <- list(
  # As the project takes up a LOT of space, all mid steps will be saved inside the cache folder of another partition,
  cachePath = file.path("/mnt/storage/borealBirdsAndForestry", "cache"),
  # while the other folders are in the working directory
  modulePath = file.path(workDirectory, "modules"),
  inputPath = file.path(workDirectory, "inputs"),
  outputPath = file.path(workDirectory, "outputs")
)

setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, outputPath = paths$outputPath, cachePath = paths$cachePath)

populationTrends <- list(
  BBWA = list(slopeSignificancy = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "BBWA_slopeSignificancy.tif"),
              slopeCoefficient = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "BBWA_slopeCoefficient.tif"),
              firstYear = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "BBWA_firstYear.tif"),
              lastYear = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "BBWA_lastYear.tif")),
  BTNW = list(slopeSignificancy = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "BTNW_slopeSignificancy.tif"),
              slopeCoefficient = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "BTNW_slopeCoefficient.tif"),
              firstYear = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "BTNW_firstYear.tif"),
              lastYear = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "BTNW_lastYear.tif")),
  CAWA = list(slopeSignificancy = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "CAWA_slopeSignificancy.tif"),
              slopeCoefficient = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "CAWA_slopeCoefficient.tif"),
              firstYear = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "CAWA_firstYear.tif"),
              lastYear = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "CAWA_lastYear.tif")),
  CMWA = list(slopeSignificancy = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "CMWA_slopeSignificancy.tif"),
              slopeCoefficient = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "CMWA_slopeCoefficient.tif"),
              firstYear = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "CMWA_firstYear.tif"),
              lastYear = file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/finalRasterPlots/data", "CMWA_lastYear.tif"))
)

polyMatrix <- matrix(c(-79.471273, 48.393518), ncol = 2)
areaSize <- 10000000
set.seed(1234)
rP <- SpaDES.tools::randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon

pathData <- file.path(getwd(), "modules", "finalRasterPlots", "data")

outputPath <- paths$outputPath
