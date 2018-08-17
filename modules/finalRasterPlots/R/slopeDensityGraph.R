slopeDensityGraph <- function(populationTrends = sim$populationTrends,
                                studyArea = sim$rP,
                                pathData = dataPath(sim),
                                outputPath = outputPath(sim)) {
  
  # This graph will for each BCR and Province combination show significant / non-significan density trends 
  # in population.
  
  finalPlotsSpecies <- lapply(X = names(populationTrends), FUN = function(species){
    
    message(crayon::yellow(paste0("Creating plot with % of density change for ", species)))
    
    slopeSig <- raster::raster(populationTrends[[species]]$slopeSignificancy)/1000
    slopeCoef <- raster::raster(populationTrends[[species]]$slopeCoefficient)/1000
    slopeCoef[slopeSig > 0.05] <- 0
    rm(slopeSig)
    
    firstYear <- raster::raster(populationTrends[[species]]$firstYear)/1000
    names(slopeCoef) <- "slopeCoefficients"
    rm(firstYear)
    gc()
    
    BCR <- reproducible::Cache(prepInputs, url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip", 
                               targetFile = "BCR_Terrestrial_master.shp",
                               archive = "bcr_terrestrial_shape.zip",
                               destinationPath = pathData,
                               studyArea = studyArea,
                               rasterToMatch = slopeCoef)
    
    BCR$id <- paste0(BCR$BCR, "_", BCR$PROVINCE_S)
    # plottingBCR$BCR_Province <- paste(plottingBCR$BCR,"_", plottingBCR$PROVINCE_S)
    
    library(viridis)
    library(ggthemes)
    library(ggplot2)
    
    speciesData <- as(slopeCoef, "SpatialPixelsDataFrame") %>%
      as.data.frame()
    
    finalPlot <- ggplot2::ggplot() +  
      geom_raster(data = speciesData, aes(x = x, y = y, fill = slopeCoef)) +
      coord_equal() +
      geom_polygon(data = BCR, aes(x = long, y = lat, group = group, colour = id), 
                   size = 1, alpha = 0) + # Colour brewer for the polygons's colours
      scale_fill_gradientn(colours = c(viridisLite::viridis(256,
                                                            alpha = 0, 
                                                            begin = 0, end = 1, 
                                                            direction = 1, 
                                                            option = "inferno"), "grey95")) +
      theme_map()
    
    browser()
    png(file.path(outputPath,paste0("densChange", species, ".png")), width = 1500, height = 863)
    finalPlot
    dev.off()
    
    return(list(plot = finalPlot, raster = slopeCoef))
    
  })
  names(finalPlotsSpecies) <- names(populationTrends)
  
  return(finalPlotsSpecies)
}
