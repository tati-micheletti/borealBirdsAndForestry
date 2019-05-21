library("raster")
library("magrittr")
CAWA1985_100m <- raster::raster("predictedCAWA100mYear1985.tif")
CAWA2011_100m <- raster::raster("predictedCAWA100mYear2011.tif")
CAWA1985_500m <- raster::raster("predictedCAWA500mYear1985.tif")
CAWA2011_500m <- raster::raster("predictedCAWA500mYear2011.tif")

CAWAdiff_100m <- ((CAWA1985_100m - CAWA2011_100m)/CAWA1985_100m)*100
CAWAdiff_500m <- ((CAWA1985_500m - CAWA2011_500m)/CAWA1985_500m)*100

raster::writeRaster(CAWAdiff_100m, filename = "diffCAWA_abund100m", format = "GTiff")
raster::writeRaster(CAWAdiff_500m, filename = "diffCAWA_abund500m", format = "GTiff")

# raster::plot(CAWAdiff_100m)
library("RColorBrewer")
library("viridis")
par(bty = 'n') 
png(file.path("borealBirdsAndForestry/outputs/predictionResults_08MAY19/", paste0("CAWA_100m.png")), 
    width = 1400, height = 700)
raster::plot(CAWAdiff_100m,
     col = viridis(n = 20, option = "B", direction = -1), #brewer.pal(n = 9, name = "YlOrRd"),#rainbow(100, start = 0.1), #viridis()
     main = paste0("Percentage of CAWA abundance reduction from\n 1985 to 2011 at local scale"), 
     axes = FALSE,
     ext = ex)
dev.off(); dev.off(); dev.off()

par(bty = 'n') 
png(file.path("borealBirdsAndForestry/outputs/predictionResults_08MAY19/", paste0("CAWA_500m.png")), 
    width = 1400, height = 700)
raster::plot(CAWAdiff_500m,
             col = viridis(n = 20, option = "B", direction = -1), #brewer.pal(n = 9, name = "YlOrRd"),#rainbow(100, start = 0.1), #viridis()
             main = paste0("Percentage of CAWA abundance reduction from\n 1985 to 2011 at neighborhood scale"), 
             axes = FALSE,
             ext = ex)
dev.off(); dev.off(); dev.off()

# Not enough memory in my laptop for this ggplot...
# plotBackcasting <- function(ras, scale){
#   ceiling_dec <- function(x, level = 1) round(x + 5*10^(-level-1), level)
#   mxVal <- ceiling_dec(max(raster::maxValue(ras)), level = 2)
#   # breaks <- seq(from = 0, to = 50, by = 5)
#   cols <- RColorBrewer::brewer.pal(n = 10, name = "Spectral")
#   dt <- raster::as.data.frame(ras,
#                               xy = TRUE, na.rm = FALSE, 
#                               long = FALSE)
#   names(dt) <- c("x", "y", "value")
#   # dtable <- data.table::data.table(dt) 
#   # dtable[, group := cut(value, breaks)]
#   # dt  <- as.data.frame(dtable)
#   # names(cols) <- unique(sort(dt$group))
#   # cols[is.na(cols)] <- "grey93"
#   library(ggplot2)
#   spPlot <- ggplot2::ggplot(data = dt, aes(x = x, y = y)) +
#     geom_tile() +
#     scale_fill_manual(
#       values = cols) +
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           panel.background = element_rect(colour = "grey93"),
#           axis.title = element_blank(),
#           legend.title = element_blank(),
#           plot.title = element_text(hjust = 0.5)) +
#     guides(fill = guide_legend(title.hjust = 0.5, reverse = TRUE)) +
#     ggtitle(label = paste0("CAWA percentage reduction from Year 1985 to 2011 at ", scale, " scale"))
#   print(spPlot)
# }
# 
# plotBackcasting(CAWAdiff_100m, "local")
# plotBackcasting(CAWAdiff_500m, "neighborhood")
