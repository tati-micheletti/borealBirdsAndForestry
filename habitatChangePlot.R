
birdList <- c("BBWA","BLPW","BOCH","BRCR","BTNW",
              "CAWA","CMWA","CONW","OVEN","PISI",
              "RBNU","SWTH","TEWA","WETA","YRWA")
scales <- c("100", "500")
wd <- "/mnt/data/Micheletti/borealBirdsAndForestry/outputs/trends"
library("reproducible")
habitatSupplyChange <- yearlyHabitatSupplyChange(species = birdList, scale = scales, 
                                                 folder = paste0(wd, scales))
habitatSupplyChange[scale == "local", sum(habitatChangePerYear)]*(2011-1985)
habitatSupplyChange[scale == "neighborhood", sum(habitatChangePerYear)]*(2011-1985)
sumBySp <- habitatSupplyChange[, sum(habitatChangePerYear), by = species]
setkey(sumBySp, V1)
orderSp <- sumBySp$species
habitatSupplyChange$species <- factor(habitatSupplyChange$species, levels = orderSp)
library("ggplot2")
setkey(habitatSupplyChange, col = "habitatChangePerYear")
plt <- ggplot(data = habitatSupplyChange, aes(x = species, y = habitatChangePerYear, fill = scale)) +
  geom_bar(stat = "identity") +
  facet_grid(scale ~ .) +
  theme(legend.position = "none")

# ~~~~~~~~~~~~~~~~ FOR NON SIGNIFICANT

