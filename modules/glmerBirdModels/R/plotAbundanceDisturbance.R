
plotAbundanceDisturbance <- function(plotList = sim$plotList){
  
  require(lme4)
  require(data.table)
  require(ggplot2)
  require(RColorBrewer)

    distProb <- data.frame(distProb = seq(from = 0, to = 1, by = 0.1))
    plotList <- plotList[rep(seq_len(nrow(plotList)), each=nrow(distProb)),]
    expanded <- cbind(plotList, distProb)
    expanded$coeff <- 1+(exp(expanded$Estimate*expanded$distProb)-1)
    
    predPlot <- ggplot(expanded, aes(x = coeff, y = distProb, col = typeDisturbance)) +
      scale_x_continuous(limits = c(0, 1)) +
      facet_grid(Species ~ disturbanceDimension) +
      geom_line(size=3) +
      theme(strip.text.y = element_text(size=12, face="bold"),
            strip.text.x = element_text(size=12, face="bold"),
            legend.position = "bottom",
            legend.title = element_text(face = "bold"),
            legend.title.align = 0.5,
            axis.text=element_text(size=12),
            axis.title=element_text(size=16,face="bold"),
            legend.text=element_text(size=12),
            panel.background = element_rect(fill = "grey99"),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      labs(x = "Proportion of disturbed area", y = "Density relative to intact areas")
    
    return(predPlot)
    
}