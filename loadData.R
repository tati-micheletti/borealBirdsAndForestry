library(raster)
year <- raster("C:/Users/tmichele/Dropbox/Project SpaDES/borealBirdsAndForestry/disturbancesData/C2C_change_year.tif")
type <- raster("C:/Users/tmichele/Dropbox/Project SpaDES/borealBirdsAndForestry/disturbancesData/C2C_change_type.tif")
nochange <- raster("C:/Users/tmichele/Dropbox/Project SpaDES/borealBirdsAndForestry/disturbancesData/C2C_Change_NoChange/C2C_change_nochange")


BC <- readRDS("C:/Users/tmichele/Dropbox/Project SpaDES/borealBirdsAndForestry/disturbancesData/shapefileBC.RData")

BC <- sp::spTransform(BC, CRS = CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
writeOGR(BC, dsn = '.', layer = 'poly', driver = "ESRI Shapefile")

