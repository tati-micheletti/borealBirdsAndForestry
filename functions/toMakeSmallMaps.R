# THIS DOESN'T WORK WELL FOR BIGGER RASTERS. NOT EFFICIENT! TOOK MORE THAN 2h TO RUN AND DIDNT EVEN FINISH!
# library("ggplot2")
# data <- raster::as.data.frame(mergedFocal2011,
#                               xy = TRUE, na.rm = FALSE, 
#                               long = FALSE)
# names(data) <- c("x", "y", "value")
# data <- data.table::data.table(data)
# p <-  ggplot(data = data, aes(x = x, y = y)) +
#   geom_tile(aes(fill = value)) +
#   coord_equal() +
#   # scale_fill_manual(values = discr_colors, labels = c(1:10, NA)) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_blank(),
#         panel.border = element_blank(),
#         axis.title = element_blank(),
#         legend.title = element_blank(),
#         plot.title = element_text(hjust = 0.5),
#         axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   ggtitle(paste0("Cummulative change in disturbance 1984-2011"))
# ggsave(filename="MAP1.png", plot = p)