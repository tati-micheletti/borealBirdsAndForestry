createBarGraphTable <- function(percDensityChange = sim$percDensityChange,
                           populationTrends = sim$populationTrends,
                           studyArea = sim$rP,
                           pathData = dataPath(sim),
                           outputPath = outputPath(sim),
                           focalDistance = max(sim$focalDistance)){

reproducible::Require("ggplot2")  
reproducible::Require("raster")
reproducible::Require("sf")
reproducible::Require("spatialEco")
reproducible::Require("dplyr")

BCR <- Cache(prepInputs, url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip", 
             targetFile = "BCR_Terrestrial_master.shp",
             archive = "bcr_terrestrial_shape.zip",
             destinationPath = pathData,
             studyArea = studyArea)
BCR$id <- paste0(BCR$BCR, "_", BCR$PROVINCE_S)
templateRas <- percDensityChange[[1]]$raster

BCRsf <- sf::st_as_sf(BCR)
BCRsf <- sf::st_transform(x = BCRsf, crs = as.character(crs(templateRas)))
ids <- transform(BCR, ID = match(id, unique(id)))
BCRsf$ID <- ids$ID
zoneBCR <- data.table::data.table(zone = BCRsf$ID, BCR_PROV = BCR$id)

BCR_PROVras <- fasterize::fasterize(sf = BCRsf, raster = templateRas, field = "ID")
BCR_PROVras <- raster::mask(x = BCR_PROVras, mask = templateRas) # excluding border BCR_PROVras pixels that don't align

  tablesNegatives <- data.table::rbindlist(lapply(X = names(percDensityChange), FUN = function(species){
    ras <- percDensityChange[[species]]$raster
    tableBCR <- data.table::data.table(zone = BCR_PROVras[], pixelValue = ras[])
    tableBCR[is.na(zone), pixelValue := NA] # excluding border ras pixels that don't align
    tableBCR[pixelValue < 0, negative := 1]
    countsPerZone <- tableBCR[,.(totalNegPixels = sum(negative, na.rm = TRUE)), by = zone]
    countsPerZone <- countsPerZone[!is.na(zone), ]
    totalPerZone <- tableBCR[,.(totalPixels = sum(!is.na(pixelValue))), by = zone]
    totalPerZone <- totalPerZone[!is.na(zone), ]
    negCalc <- merge(totalPerZone, countsPerZone)
    negCalc[, percentNegative := totalNegPixels/totalPixels]
    tableNegativeSpecies <- data.table::data.table(species = species, 
                                       percentNegative = negCalc$percentNegative,
                                       zone = negCalc$zone,
                                       scale = focalDistance)
    return(tableNegativeSpecies)
    })
  )
  write.csv(x = tablesNegatives, file = file.path(outputPath, paste0("tableNegatives", focalDistance, ".csv")))
return(list(tablesNegatives = tablesNegatives, zoneBCR = zoneBCR))
}
