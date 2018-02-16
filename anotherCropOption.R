# ------------------- Crop to shapefile ----------------------------
# OPTION 1: CROP TO THE SHAPEFILE EXTENTE AND THEN MASK TO SHAPEFILE
# Using gdal_translate
raster.path <-file.path(getwd(),raster.name) # Set the raster path
shp <- testArea # Load shapefile
outfile <- "cropped.tif" # set the output file
ext <- extent(shp) # set the extent of the crop area 

gdal_translate(raster.path, outfile, projwin=c(xmin(ext), ymax(ext), xmax(ext), ymin(ext))) # Crop
ras <- raster(file.path(getwd(),"cropped.tif")) # Retrieve the saved file
ras[] <- ras[]

# OPTION 2: MASK DIRECTLY TO THE SHAPEFILE
# Using raster::mask

#Microbenchmarking both
microbenchmark::microbenchmark(
  mr1 <- mask(ras, shp),
  mr2 <- mask(LCC.raster, shp)
)

