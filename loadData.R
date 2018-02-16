
library(raster)
library(rgdal)
library(gdalUtils)
library(sf)

# Load the shapefile for masking
testArea <- readOGR(file.path(getwd(),"data/testArea/testArea.shp"))
testArea <- testArea[testArea$OBJECTID_1==1,]

#Load the disturbances files (at national scale)
yearOriginal <- raster(file.path(getwd(),"data/C2C_Change_Year/C2C_change_year.tif"))
typeOriginal <- raster(file.path(getwd(),"data/C2C_Change_Type/C2C_change_type.tif"))
nochangeOriginal <- raster(file.path(getwd(),"data/C2C_Change_NoChange/C2C_change_nochange.tif"))

#Crop the national map to the shapefile
#Cache(mask, x = yearOriginal, mask = testArea, filename="C2C_year.tif") #Too slow

 cutline <- as.character(file.path(getwd(),"data/testArea/testArea.shp"))
 year <- "C2C_year.tif"
 
# ----- WITH CACHE -------- NOT WORKING, not creating the file
# Cache(gdalwarp, overwrite=TRUE, cutline = cutline,
#       dstalpha = TRUE,
#       s_srs = as.character(crs(yearOriginal)),
#       t_srs = as.character(crs(yearOriginal)),
#       multi=TRUE, of="GTiff",
#       crop_to_cutline = TRUE, tr=c(250, 250),
#       filename(yearOriginal), ot = "CFloat64",
#       year,
#       cacheRepo=paths$cachePath)

# ----- WITHOUT CACHE --------# 
gdalwarp(overwrite=TRUE, cutline = cutline,
dstalpha = TRUE,
s_srs= as.character(crs(yearOriginal)),
t_srs= as.character(crs(yearOriginal)),
multi=TRUE, of="GTiff",
crop_to_cutline = TRUE, tr=c(30, 30),
filename(yearOriginal), ot = "CFloat64",
year)

testArea.ras <- raster::stack(file.path(getwd(),"C2C_year.tif"))

testArea.ras <- setMinMax(testArea.ras)
tA1 <- raster(testArea.ras$C2C_year.1)
tA1 <- setMinMax(tA1)
tA2 <- raster(testArea.ras$C2C_year.2)
tA2 <- setMinMax(tA2)
