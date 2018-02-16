
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

# FAILS: Returns error
 cutline <- as.character(file.path(getwd(),"data/testArea/testArea.shp"))
 Cache(gdalwarp, srcfile = "C2C_change_year.tif", dstfile = "C2C_year.tif",
         s_srs = as.character(crs(yearOriginal)), t_srs = as.character(crs(yearOriginal)),
         extent(yearOriginal), tr = res(yearOriginal),
         dstalpha = TRUE, multi = TRUE, of = "GTiff", cutline = cutline,
         crop_to_cutline = TRUE, overwrite = TRUE,
         output_Raster = TRUE)

# FAILS: Returns NULL
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
crop_to_cutline = TRUE, tr=c(250, 250),
filename(yearOriginal), ot = "CFloat64",
year)
 
testArea.ras <- raster(file.path(getwd(),"cache/C2C_year.tif"))
