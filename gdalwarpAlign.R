library(raster)    
## Create a matrix with random data & use image(
# fakeTemplate <- raster::raster("/mnt/storage/borealBirdsAndForestry/cache/land/fakeTemplate.gri")
# writeRaster(x = fakeTemplate, filename = fakeBirdDensityPath)
# lc <- projectRaster(res = c(30, 30), from = fakeBirdDensity, method = "ngb",
#                     crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

fakeBirdDensityPath <- "/mnt/storage/borealBirdsAndForestry/cache/land/fakeBirdDensity.tif"
fakeBirdDensity <- raster::raster(fakeBirdDensityPath)
# writeRaster(x = fakeBirdDensity, filename = fakeBirdDensityPath)

fakeLandCoverPath <- "/mnt/storage/borealBirdsAndForestry/cache/land/fakeLandCover.tif"
fakeLandCover <- raster::raster(fakeLandCoverPath)
# writeRaster(x = fakeLandCover, filename = fakeLandCoverPath, overwrite = TRUE)

testReprojTile <- file.path("/mnt/storage/borealBirdsAndForestry/cache/land", "NEWfakeLandCover.tif")
tr <- res(fakeBirdDensity)

system( # This will be replaced by prepInputs when it can handle large files with gdalwarp system call
  paste0(paste0(getOption("gdalUtils_gdalPath")[[1]]$path, "gdalwarp "),
         "-s_srs \"", as.character(raster::crs(fakeLandCover)), "\"",
         " -t_srs \"", as.character(raster::crs(fakeBirdDensity)), "\"",
         " -multi ",
         "-wo NUM_THREADS=35 ",
         "-ot UInt16 ",
         "-overwrite ",
         "-tr ", paste(tr, collapse = " "), " ",
         fakeLandCoverPath, " ",
         testReprojTile),
  wait = TRUE)

test <- raster::raster(testReprojTile)
newTest <- raster::crop(test, fakeBirdDensity)
raster::extent(newTest) <- raster::alignExtent(extent = raster::extent(newTest), object = fakeBirdDensity, snap = "near")

raster::compareRaster(newTest, fakeBirdDensity, extent = TRUE)
