# TESTING GDALWRAP FUNCTION

library(raster)
library(rgdal)
library(quickPlot)
library(gdalUtils)

LCC.raster <- raster(file.path(getwd(),"data/can_age04_1km.tif"))
LCC.path <- file.path(getwd(),"data/can_age04_1km.tif")
data.path <- file.path(getwd(),"data")
LCC.raster[] <- LCC.raster[] # Bring raster to memory

#testArea <- sf::st_read(file.path(getwd(),"data/testArea/testArea.shp"))
testArea <- readOGR(file.path(getwd(),"data/testArea.shp"))

#Transform to the same projection
#testArea <- sf::st_transform(x = testArea, crs(LCC.raster)) # cannot create a crs from an object of class CRS
testArea <- sp::spTransform(x = testArea, CRSobj = crs(LCC.raster)) #"+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") # Doesn't work, can't convert crs? 

# Visualize data
plot(LCC.raster);plot(testArea, add=TRUE)

cutline <- as.character(file.path(getwd(),"data/testArea.shp"))

# Masking the raster to a shapefile (obs.: assigning the function doesn't return the object)
gdalwarp(srcfile = LCC.path, dstfile = "croppedMap.tif", overwrite=TRUE, 
                    cutline = cutline,
                    dstalpha = TRUE,
                    s_srs= as.character(crs(LCC.raster)),
                    t_srs= as.character(crs(LCC.raster)),
                    multi=TRUE, of="GTiff",
                    crop_to_cutline = TRUE, tr=c(1000, 1000))

# Testing the cropping
croppedArea <- raster(file.path(getwd(),"croppedMap.tif"))
croppedArea[] <- croppedArea[]
plot(croppedArea)

