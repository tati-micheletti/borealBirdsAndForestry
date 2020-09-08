
plotCoefficients2 <- function(sim = sim, plotList = sim$plotList){
  
  ## got these from: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R
  ## arrange Estimate by species within combination of typeDisturbance and disturbanceDimension
  
  reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }
  
  scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
  }
  
  scale_y_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_y_discrete(labels = function(x) gsub(reg, "", x), ...)
  }
  
  require(ggplot2)
  require(ggalt)
  require(dplyr)
  
  plot <- ggplot(data = plotList, aes(y = Estimate, 
                                      x = reorder_within(Species, by = Estimate, within = paste0(typeDisturbance, disturbanceDimension)))) +
    geom_point(data = plotList, aes(x = Estimate, fill = as.factor(Significancy)), pch=21) +
    scale_fill_manual(values = c("YES" = "black", "NO" = "white")) +
    geom_linerange(aes(ymin = lowerCI, ymax = upperCI)) +
    geom_hline(yintercept = 0, linetype="dashed", color="darkgrey") +
    scale_x_reordered() +
    theme(strip.text.y = element_text(size=12, face="bold"),
          strip.text.x = element_text(size=12, face="bold"),
          legend.title = element_text(face = "bold"),
          axis.text.y = element_text(size=7),
          axis.text=element_text(size=12),
          axis.title=element_text(size=16,face="bold"),
          panel.background = element_rect(fill = "grey99"),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    labs(y = "Abundance estimates", x = "Bird species") +
    coord_flip() +
    facet_grid(disturbanceDimension ~ typeDisturbance, scales = "free_y") #, scales = "free_y"
  
  png(file.path(sim@paths$outputPath,"plotCoefficients2.png"), width = 1500, height = 863)
  plot
  dev.off()
  
  return(plot)
  
}
