createBarGraph <- function(percDensityChange = sim$percDensityChange, 
                           populationTrends = sim$populationTrends,
                           studyArea = sim$rP,
                           pathData = dataPath(sim),
                           outputPath = outputPath(sim)){
  
reproducible::Require("raster")
reproducible::Require("sf")
reproducible::Require("spatialEco")

BCR <- Cache(prepInputs, url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip", 
             targetFile = "BCR_Terrestrial_master.shp",
             archive = "bcr_terrestrial_shape.zip",
             destinationPath = pathData,
             studyArea = studyArea)

PROV <- Cache(prepInputs, url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip", 
              targetFile = "gpr_000b11a_e.shp",
              archive = "gpr_000b11a_e.zip",
              destinationPath = pathData,
              studyArea = studyArea)

BCR_PROV <- raster::intersect(BCR, PROV)
browser()
fortBCR_PROV <- fortify(BCR_PROV)
BCR_PROV$id <- row.names(BCR_PROV)

templateRas <- percDensityChange[[species]]$raster

BCR_PROVsf <- sf::st_as_sf(BCR_PROV) %>%
  sf::st_transform(x = BCR_PROVsf, crs = as.character(crs(templateRas)))

BCR_PROV <- sf:::as_Spatial(BCR_PROVsf)

  tablesNegatives <- lapply(X = names(percDensityChange), FUN = function(species){
    (percDensityChange[[species]]$raster)
    tableNegativeSpecies <- data.frame("negativePercent" = spatialEco::zonal.stats(x = BCR_PROV,
                                                                                   y = percDensityChange[[species]]$raster, 
                                                                                   stat = returnPercentNegative), ID = BCR_PROV$BCR)
    return(tableNegativeSpecies)
  })
  names(tablesNegatives) <- names(percDensityChange)
  
  newNegTable <- rbindlist(lapply(names(tablesNegatives), function(x){
    tablesNegatives[[x]]$SPECIES <- x
    return(tablesNegatives[[x]])
  }))

  browser()
  # NOW I NEED TO PUT newNegTable into a bar graph!
  
return(barPlot)
}
