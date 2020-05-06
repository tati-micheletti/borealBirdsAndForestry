tileList <- tiles$Raster1
# library(raster)
# library(data.table)
# totalPixels <- lapply(seq_along(tileList), function(rasN){
#   ras <- tileList[[rasN]]
#   # bring raster to memory
#   ras[] <- ras[]
#   tb <- table(ras[])
#   dt <- data.table(typeDisturbance = as.numeric(names(tb)),
#                    totalPixels = as.numeric(tb),
#                    tile = paste0("tile", rasN))
#   dt <- dt[typeDisturbance != 0, ]
#   print(paste0("Table Finished for tile ", rasN))
#   return(dt)
# })
# tb <- copy(rbindlist(totalPixels))
tb <- readRDS(file.path(getwd(), "outputs/tableDisturbanceArea.rds"))
tb2 <- tb[, sum(totalPixels), by = "typeDisturbance"]
names(tb2) <- c("typeDisturbance", "nPixels")
pixelArea <- 30*30
tb2$areaM2 <- tb2$nPixels*pixelArea
tb2$areaHa <- tb2$areaM2*0.0001

# managedForest <- data.table(managedForest@data)

# tb2$managedBorealForest <-  440662029 # Calculated from the managed shapefile 
# tb2$totalBorealForest <-  440662029+548721094.65 # Calculated from the managed shapefile 

# tb2$totalBorealForest <-  270000000 # Using data from https://www.nrcan.gc.ca/our-natural-resources/forests-forestry/sustainable-forest-management/boreal-forest/13071
# tb2$managedBorealForest <-  0.4453907*tb2$totalBorealForest # Calculated using the proportion

# tb2$totalBorealForest <-  307000000 # Using data from W&W
# tb2$managedBorealForest <-  0.4453907*tb2$totalBorealForest # Calculated using the proportion based in the shapefile ---> def not precise

# tb2$totalBorealForest <-  307000000 # Using data from W&W
tb2$managedBorealForest <-  57500000 # Calculated using the proportion based in the shapefile ---> def not precise

tb2[, managedDistProp := areaHa/managedBorealForest]
tb2$managedDistPerc <- tb2$managedDistProp*100

tb2[, totalDistProp := areaHa/totalBorealForest]
tb2$totalDistPerc <- tb2$totalDistProp*100
tb2 <- tb2[typeDisturbance %in% c(1, 2)]
tb2$typeDisturbance <- c("Fire", "Forestry")
tb2[, c("areaM2", "managedDistProp", "totalDistProp") := NULL]
knitr::kable(tb2)
# 548721094.65 Non managed. Last question: are all pixels disturbed in the total forest, or managed?