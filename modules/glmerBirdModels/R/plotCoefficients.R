
plotCoefficients <- function(sim = sim, plotList = sim$plotList){
  
  require(ggplot2)
  require(ggalt)
  
  part <- plotList[plotList$typeDisturbance=="LOCAL"&plotList$disturbanceDimension=="TRANSITIONAL",]
  extc <- part$Species
  plotList$newSpecies = factor(plotList$Species, levels=extc, labels=extc)

  plot <- ggplot(plotList, mapping = aes(x = Estimate, y = newSpecies)) +
    facet_grid(typeDisturbance ~ disturbanceDimension, scales = "free_y") +
    geom_segment(aes(x = lowerCI, xend = upperCI, yend = Species)) +
    geom_point(aes(x = Estimate), pch=ifelse(plotList$p < 0.05, 19, 21), 
               fill=ifelse(plotList$p < 0.05, "black", "white")) +
    theme(strip.text.y = element_text(size=12, face="bold"),
          strip.text.x = element_text(size=12, face="bold"),
          legend.title = element_text(face = "bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=16,face="bold"),
          panel.background = element_rect(fill = "grey99"),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    labs(x = "Abundance estimates", y = "Bird species") +
    geom_vline(xintercept = 0, linetype="dashed", color="darkgrey")
  
  png(file.path(sim@paths$outputPath,"plotCoefficients.png"), width = 1500, height = 863)
  plot
  dev.off()
  
  return(plot)
  
}