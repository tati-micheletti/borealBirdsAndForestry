library(raster)
shp <- shapefile("C:/Users/tmichele/GitHub/borealBirdsAndForestry/modules/bayesianBirdModel/data/BEAD_2010.shp")
loss2001 <- shapefile("C:/Users/tmichele/Dropbox/Anthropogenic_disturbances_paper ACE-ECO/gisdata/GFW/Without burnt polygons/lossyear_2001.shp")
loss2002 <- shapefile("~/gisdata/GFW/Without burnt polygons/lossyear_2002.shp")

# Subset by year of disturbance (both datsets bird and GIS)
# Calculated accumulative disturbances for each year