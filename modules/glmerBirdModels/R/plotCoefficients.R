plotCoefficients <- function(outputPath = outputPath(sim), 
                              plotList = sim$plotList){
  
  # THESE ARE THE MODIFICATIONS ASKED FROM ALBERTO ON 23rd MAY 2018 + 31st May

    require(ggplot2)
    require(ggalt)
    
  if ("LOCAL" %in% unique(plotList$disturbanceDimension) &  # Re-write better. For organizing the plot
      "TRANSITIONAL" %in% unique(plotList$typeDisturbance)) {
    part <- plotList[plotList$disturbanceDimension == "LOCAL" & plotList$typeDisturbance == "TRANSITIONAL",]
  } else {
    if ("LOCAL" %in% unique(plotList$disturbanceDimension) & 
        "BOTH" %in% unique(plotList$typeDisturbance)) { 
      part <- plotList[plotList$disturbanceDimension == "LOCAL" & plotList$typeDisturbance == "BOTH",]
    } else {
      if ("LOCAL" %in% unique(plotList$disturbanceDimension) & 
          "PERMANENT" %in% unique(plotList$typeDisturbance)) { 
        part <- plotList[plotList$disturbanceDimension == "LOCAL" & plotList$typeDisturbance == "PERMANENT",]
      } else {
        if ("NEIGHBORHOOD" %in% unique(plotList$disturbanceDimension) & 
            "TRANSITIONAL" %in% unique(plotList$typeDisturbance)) { 
          part <- plotList[plotList$disturbanceDimension == "NEIGHBORHOOD" & plotList$typeDisturbance == "TRANSITIONAL",]
        } else {
          if ("NEIGHBORHOOD" %in% unique(plotList$disturbanceDimension) & 
              "BOTH" %in% unique(plotList$typeDisturbance)) { 
            part <- plotList[plotList$disturbanceDimension == "NEIGHBORHOOD" & plotList$typeDisturbance == "BOTH",]
          } else {
            if ("NEIGHBORHOOD" %in% unique(plotList$disturbanceDimension) & 
                "PERMANENT" %in% unique(plotList$typeDisturbance)) { 
              part <- plotList[plotList$disturbanceDimension == "NEIGHBORHOOD" & plotList$typeDisturbance == "PERMANENT",]
            } else {
              if ("LOCAL UNDISTURBED" %in% unique(plotList$disturbanceDimension) & 
                  "TRANSITIONAL" %in% unique(plotList$typeDisturbance)) { 
                part <- plotList[plotList$disturbanceDimension == "LOCAL UNDISTURBED" & plotList$typeDisturbance == "TRANSITIONAL",]
              } else {
                if ("LOCAL UNDISTURBED" %in% unique(plotList$disturbanceDimension) & 
                    "BOTH" %in% unique(plotList$typeDisturbance)) { 
                  part <- plotList[plotList$disturbanceDimension == "LOCAL UNDISTURBED" & plotList$typeDisturbance == "BOTH",]
                } else {
                  if ("LOCAL UNDISTURBED" %in% unique(plotList$disturbanceDimension) & 
                      "PERMANENT" %in% unique(plotList$typeDisturbance)) { 
                    part <- plotList[plotList$disturbanceDimension == "LOCAL UNDISTURBED" & plotList$typeDisturbance == "PERMANENT",]
                  } else {
                    
                  }
                }
              }
              
            }
          }
        }
        
      }
    }
  }

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
    
    browser()
    # increase size of the x and y axes labels.
    # Increase the weight or width of the lines.
    # Make two separate graphs. Also, it's the tix legends "BTNW", "-1" that need to be bigger; the axis labels could also be bigger.

    plotListSuc <- plotList[plotList$newTypeDisturbance == "SUCCESSIONAL DISTURBANCES",]
    plotListSuc$newTypeDisturbance <- factor(plotListSuc$newTypeDisturbance)
    plotListSuc$typeDisturbance <- factor(plotListSuc$typeDisturbance)
    plotListSuc$disturbanceDimension <- factor(plotListSuc$disturbanceDimension)
    
    plotListAlie <- plotList[plotList$newTypeDisturbance == "ALIENATING DISTURBANCES",]
    plotListAlie$newTypeDisturbance <- factor(plotListAlie$newTypeDisturbance)
    plotListAlie$typeDisturbance <- factor(plotListAlie$typeDisturbance)
    plotListAlie$disturbanceDimension <- factor(plotListAlie$disturbanceDimension)
      
    plot <- ggplot(plotListAlie, mapping = aes(x = Estimate, y = newSpecies)) +
      facet_grid(disturbanceDimension ~ newTypeDisturbance, scales = "free_y") + #Changed this line
      geom_segment(aes(x = lowerCI, xend = upperCI, yend = Species, size = 2)) +
      geom_point(data = plotListAlie, aes(x = Estimate, fill = as.factor(Significancy), size = 15), pch=21) +
      scale_fill_manual(values = c("YES" = "black", "NO" = "white")) +
      theme(legend.position = "none",
            strip.text.y = element_text(size=18, face="bold"), # changed this on 31st May
            strip.text.x = element_text(size=18, face="bold"), # changed this on 31st May
            legend.title = element_text(face = "bold"),
            axis.text=element_text(size=22),
            axis.title=element_text(size=20,face="bold"),
            panel.background = element_rect(fill = "grey99"),
            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
      labs(x = "Model coefficients", y = "Bird species") +
      geom_vline(xintercept = 0, linetype="dashed", color="darkgrey")

    png(file.path(outputPath,"plotCoefficients.png"), width = 1500, height = 863)
    plot
    dev.off()
    
    return(plot)
    
  }
  