populationTrends <- list(
  BBWA = list(slopeSignificancy = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "BBWA_slopeSignificancy.tif"),
              slopeCoefficient = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "BBWA_slopeCoefficient.tif"),
              firstYear = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "BBWA_firstYear.tif"),
              lastYear = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "BBWA_lastYear.tif")),
  BTNW = list(slopeSignificancy = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "BTNW_slopeSignificancy.tif"),
              slopeCoefficient = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "BTNW_slopeCoefficient.tif"),
              firstYear = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "BTNW_firstYear.tif"),
              lastYear = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "BTNW_lastYear.tif")),
  CAWA = list(slopeSignificancy = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "CAWA_slopeSignificancy.tif"),
              slopeCoefficient = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "CAWA_slopeCoefficient.tif"),
              firstYear = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "CAWA_firstYear.tif"),
              lastYear = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "CAWA_lastYear.tif")),
  CMWA = list(slopeSignificancy = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "CMWA_slopeSignificancy.tif"),
              slopeCoefficient = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "CMWA_slopeCoefficient.tif"),
              firstYear = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "CMWA_firstYear.tif"),
              lastYear = file.path("/mnt/storage/borealBirdsAndForestry", "cache", "outputRasters", "CMWA_lastYear.tif"))
  )

polyMatrix <- matrix(c(-79.471273, 48.393518), ncol = 2)
areaSize <- 10000000
set.seed(1234)
rP <- SpaDES.tools::randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon

pathData <- file.path(getwd(), "modules", "finalRasterPlots", "data")

outputPath <- "/home/tmichele/Documents/GitHub/borealBirdsAndForestry/outputs"
