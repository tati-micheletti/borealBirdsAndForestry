ras1 <- raster::raster(file.path(getwd(), "dummyRasters", "rasterNumber1.tif"))
merged <- SpaDES.tools::mergeRaster(splitted) # Merge .gri # Doesn't work


splitted <- SpaDES.tools::splitRaster(r = ras1, nx = 3, ny = 3, 
                                      buffer = c(1500, 1500), rType = "INT1U", 
                                      path = file.path(getwd(), "dummyRasters"))

merged1 <- mergeRaster(x = splitted) # merged direto

allLists2 <- list.files(path = file.path(getwd(), "dummyRasters"), 
                       pattern = "gri$", full.names = TRUE)

allLists3 <- list.files(path = file.path(getwd(), "dummyRasters"), 
                        pattern = "grd$", full.names = TRUE)

merged2 <- mergeRaster(list(allLists2)) # Merge .gri # Doesn't work
merged3 <- mergeRaster(list(allLists3)) # Merge .grd # Doesn't work

# Save the paths, and when I need to recover the paths to merge, read all rasters into a list
listRecovered <- lapply(X = allLists3, FUN = raster::raster)

