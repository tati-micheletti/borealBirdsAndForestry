plotAbundanceDisturbance2 <- function(sim = sim, plotList = sim$plotList){
  
  # Significancy here relates to each curve in relation to the others in the same plot.
  # Still needs to be done. Depends on the not yet written function anovaModels.R.
  # 
  # require(lme4)
  # require(data.table)
  # require(ggplot2)
  # require(RColorBrewer)
  # 
  # distProb <- data.frame(distProb = seq(from = 0, to = 1, by = 0.1))
  # plotList <- plotList[rep(seq_len(nrow(plotList)), each=nrow(distProb)),]
  # expanded <- cbind(plotList, distProb)
  # expanded$coeff <- exp(expanded$Estimate*expanded$distProb)
  # 
  # predPlot <- ggplot(expanded, aes(x = distProb, y = coeff, col = typeDisturbance)) +
  #   scale_colour_grey(start = 0.2, end = 0.8, name = "Type of Disturbance") +
  #   scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c(0, 50, 100)) +
  #   scale_y_continuous(limits = c(0, 2)) +
  #   facet_grid(disturbanceDimension ~ Species) +
  # geom_line(size=1.2, aes(linetype = as.factor(Significancy))) +
  #   scale_linetype_manual(values = c("YES" = "dashed", "NO" = "solid")) +
  #   theme(strip.text.y = element_text(size=12, face="bold"),
  #         strip.text.x = element_text(size=12, face="bold"),
  #         legend.key = element_blank(),
  #         legend.position = "bottom",
  #         legend.title = element_text(face = "bold"),
  #         legend.title.align = 0.5,
  #         axis.text=element_text(size=10),
  #         axis.title=element_text(size=16,face="bold"),
  #         legend.text=element_text(size=12),
  #         panel.background = element_rect(fill = "grey99"),
  #         panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  #   labs(x = "Percentage of disturbed area", 
  #        y = "Density relative to intact areas")
  # 
  # png(file.path(sim@paths$outputPath,"plotAbundanceDisturbance2.png"), width = 1500, height = 863)
  # predPlot
  # dev.off()
  # 
  # return(predPlot)
  
}
