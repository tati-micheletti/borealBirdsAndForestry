plotCoefficients3 <- function(outputPath = outputPath(sim), sim = sim, plotList = sim$plotList){
  
  # THESE ARE THE MODIFICATIONS ASKED FROM ALBERTO ON 23rd MAY 2018 + 31st May

    require(ggplot2)
    require(ggalt)
    
    part <- plotList[plotList$disturbanceDimension=="LOCAL"&plotList$typeDisturbance=="TRANSITIONAL",]
    extc <- part$Species
    plotList$newSpecies <- factor(plotList$Species, levels=extc, labels=extc)

    for(i in 1:nrow(plotList)){ # Changed this
      if(plotList$typeDisturbance[i] == "PERMANENT"){
        plotList$newTypeDisturbance[i] <- "ALIENATING DISTURBANCES"
      } else {
        if(plotList$typeDisturbance[i] == "TRANSITIONAL"){
          plotList$newTypeDisturbance[i] <- "SUCCESSIONAL DISTURBANCES"
        } else{
          plotList$newTypeDisturbance[i] <- "COMBINED DISTURBANCES"
        }
      }
    }
    
    plotList$newTypeDisturbance <- factor(plotList$newTypeDisturbance, levels = c("SUCCESSIONAL DISTURBANCES", "ALIENATING DISTURBANCES", "COMBINED DISTURBANCES")) # Changed this
    
    plot <- ggplot(plotList, mapping = aes(x = Estimate, y = newSpecies)) +
      facet_grid(disturbanceDimension ~ newTypeDisturbance, scales = "free_y") + #Changed this line
      geom_segment(aes(x = lowerCI, xend = upperCI, yend = Species)) +
      geom_point(data = plotList, aes(x = Estimate, fill = as.factor(Significancy)), pch=21) +
      scale_fill_manual(values = c("YES" = "black", "NO" = "white")) +
      theme(legend.position = "none",
            strip.text.y = element_text(size=16, face="bold"), # changed this on 31st May
            strip.text.x = element_text(size=16, face="bold"), # changed this on 31st May
            legend.title = element_text(face = "bold"),
            axis.text=element_text(size=12),
            axis.title=element_text(size=16,face="bold"),
            panel.background = element_rect(fill = "grey99"),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      labs(x = "Model coefficients", y = "Bird species") +
      geom_vline(xintercept = 0, linetype="dashed", color="darkgrey")
    
    png(file.path(outputPath,"plotCoefficients.png"), width = 1500, height = 863)
    plot
    dev.off()
    
    return(plot)
    
  }
  