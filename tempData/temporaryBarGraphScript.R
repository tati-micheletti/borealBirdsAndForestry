createBarGraph <- function(BarGraphTableLocal = "C:/Users/tmichele/GitHub/borealBirdsAndForestry/borealBirdsAndForestryManuscripts/IOC 2018 Poster/Neighborhood/tableNegatives100.csv",
                           BarGraphTableNeighborhood = "C:/Users/tmichele/GitHub/borealBirdsAndForestry/borealBirdsAndForestryManuscripts/IOC 2018 Poster/Neighborhood/tableNegatives500.csv",
                           zoneBCR = mySimOut$BarGraphTable$zoneBCR){
  
  # When data is ready, use these two lines and delete the MAKIG FAKE
  # neighborhood <- read.csv(file = "/home/tmichele/Documents/GitHub/borealBirdsAndForestry/outputs/tableNegatives500")
  # local <- read.csv(file = "/home/tmichele/Documents/GitHub/borealBirdsAndForestry/outputs/tableNegatives100")

  neighborhood <- data.table::fread(file = "C:/Users/tmichele/GitHub/borealBirdsAndForestry/borealBirdsAndForestryManuscripts/IOC 2018 Poster/Neighborhood/tableNegatives500.csv")
# ==== MAKING FAKE LOCAL UNTIL DATA IS READY ==== #
  local <- neighborhood
  local$percentNegative <- local$percentNegative + runif(n = nrow(x = local), min = 0.05, max = 0.1)
  local$scale <- 100
  # ====================================== #
  
  # FIX WHEN IT COMES TO THE REAL COUNTRY BACKCASTING
  polyMatrix <- matrix(c(-79.471273, 48.393518), ncol = 2)
  areaSize <- 10000000
  set.seed(1234)
  rP <- SpaDES.tools::randomPolygon(x = polyMatrix, hectares = areaSize) # Create Random polygon
  BCR <- reproducible::prepInputs(url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip",
               targetFile = "BCR_Terrestrial_master.shp",
               archive = "bcr_terrestrial_shape.zip",
               destinationPath = getwd(),
               studyArea = rP)
  BCR$id <- paste0(BCR$BCR, "_", BCR$PROVINCE_S)
  BCRsf <- sf::st_as_sf(BCR)
  ids <- transform(BCR, ID = match(id, unique(id)))
  BCRsf$ID <- ids$ID
  zoneBCR <- data.table::data.table(zone = BCRsf$ID, BCR_PROV = BCR$id)
  
  dt <- rbind(local, neighborhood)
  
  dt[, BCR_PROV := lapply(zone, FUN = function(x){
    zn <- zoneBCR[zone == x, BCR_PROV]
    return(zn)
  })]
  
  df <- as.data.frame(dt)
  df$BCR_PROV <- unlist(df$BCR_PROV)
  df$scale <- as.factor(df$scale)
  
  library("ggplot2")
  library("viridis")
  
  labelFacets <- c("100" = "Local", "500" = "Neighborhood")
  plot <- ggplot(data = df, aes(x = BCR_PROV, y = percentNegative, fill = species)) +
    facet_grid(scale ~ ., labeller = as_labeller(labelFacets)) +
    geom_bar(stat = "identity", position = position_dodge())+
    scale_fill_viridis(discrete = TRUE, option = "D") + 
    theme(legend.position = "none",
          strip.text.y = element_text(size = 28, face = "bold"),
          axis.title.x = element_text(size = 30), 
          axis.title.y = element_text(size = 30),
          axis.text.x = element_text(size = 28), 
          axis.text.y = element_text(size = 28)) +
    scale_x_discrete(name = "Combination BCR and Province") +
    scale_y_continuous(name = "% of pixels showing negative trends") +
    labs(fill = "Bird species")
  
  browser()
  # png(file.path("C:/Users/tmichele/GitHub/borealBirdsAndForestry/borealBirdsAndForestryManuscripts/IOC 2018 Poster", 
  #               paste0("barGraph.png")), width = 2500, height = 1500) # FOR TESTING ON LOCAL MACHINE ONLY
  png(file.path(outputPath, paste0("barGraph.png")), width = 2500, height = 1500)
  plot
  dev.off()
  
  return()
}
