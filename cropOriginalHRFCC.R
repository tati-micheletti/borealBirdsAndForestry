# TESTING GDALWRAP FUNCTION

library(raster)
library(rgdal)
library(quickPlot)
library(gdalUtils)

year.ras <- raster(file.path(getwd(),"data/C2C_Change_Year/C2C_change_year.tif"))
year.ras.path <- file.path(getwd(),"data/C2C_Change_Year/C2C_change_year.tif")

type.ras <- raster(file.path(getwd(),"data/C2C_Change_Type/C2C_change_type.tif"))
type.ras.path <- file.path(getwd(),"data/C2C_Change_Type/C2C_change_type.tif")

nochange.ras <- raster(file.path(getwd(),"data/C2C_Change_NoChange/C2C_change_nochange.tif"))
nochange.ras.path <- file.path(getwd(),"data/C2C_Change_NoChange/C2C_change_nochange.tif")

data.path <- file.path(getwd(),"data")
year.ras[] <- year.ras[] # Bring raster to memory

#testArea <- sf::st_read(file.path(getwd(),"data/testArea/testArea.shp"))
testArea <- readOGR(file.path(getwd(),"data/testArea.shp"))

# Visualize data
#plot(year.ras);plot(testArea, add=TRUE)

cutline <- as.character(file.path(getwd(),"data/testArea.shp"))

# Masking the raster to a shapefile (obs.: assigning the function doesn't return the object)
gdalwarp(srcfile = year.ras.path, dstfile = "croppedYear.tif", overwrite=TRUE, 
         cutline = cutline,
         dstalpha = TRUE,
         s_srs= as.character(crs(year.ras)),
         t_srs= as.character(crs(year.ras)),
         multi=TRUE, of="GTiff",
         crop_to_cutline = TRUE, tr=c(30, 30))

gdalwarp(srcfile = type.ras.path, dstfile = "croppedType.tif", overwrite=TRUE, 
         cutline = cutline,
         dstalpha = TRUE,
         s_srs= as.character(crs(type.ras)),
         t_srs= as.character(crs(type.ras)),
         multi=TRUE, of="GTiff",
         crop_to_cutline = TRUE, tr=c(30, 30))

gdalwarp(srcfile = nochange.ras.path, dstfile = "croppedNC.tif", overwrite=TRUE, 
         cutline = cutline,
         dstalpha = TRUE,
         s_srs= as.character(crs(nochange.ras)),
         t_srs= as.character(crs(nochange.ras)),
         multi=TRUE, of="GTiff",
         crop_to_cutline = TRUE, tr=c(30, 30))

# Testing the cropping
croppedYear <- raster(file.path(getwd(),"croppedYear.tif"))
croppedYear[] <- croppedYear[]
plot(croppedYear)

croppedType <- raster(file.path(getwd(),"croppedType.tif"))
croppedType[] <- croppedType[]
plot(croppedType)

croppedNC <- raster(file.path(getwd(),"croppedNC.tif"))
croppedNC[] <- croppedNC[]
plot(croppedNC)