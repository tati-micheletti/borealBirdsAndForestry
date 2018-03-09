# Plotting results for Alberto's models
#dataset = sim$data

#to call the function

typeDisturbance <- mySim$typeDisturbance[1:2]
disturbanceDimension <- mySim$disturbanceDimension
dataset <- mySim$data

plotDisturbanceSector <- function(dataset, typeDisturbance, disturbanceDimension){
  require(ggplot2)
  require(reproducible)
  
  #plotDist <- list()
  plotDist <- lapply(X = dataset, FUN = function(x){
    
    browser()
    
    listName <- names(dataset[x])
    
    
  })
    
    
    {
    
    listName <- names(dataset[x])
    state <- ifelse(grepl(listName, "local"),"100","500")

      DT <- subset(x, eval(parse(text = paste0("State_P_",state)))>0)
      legend <- factor(eval(parse(text = paste0("DT$", "Agent_", state))))
      
      graph <- qplot(eval(parse(text = paste0("DT$","State_P_",state))), 
                    geom="histogram", 
                    fill=legend, 
                    xlab="Proportion of disturbed area, local scale", 
                    ylab="Number of surveys") +
                theme(axis.text=element_text(size=12), 
                      axis.title=element_text(size=16,face="bold"),
                      legend.text=element_text(size=12), 
                      legend.title=element_text(size=14))
      return(graph)
  })
  
  
  for (dD in disturbanceDimension){
    for (tD in typeDisturbance){
    
      state <- ifelse (dD=="local","100","500")
      
        DT <- eval(parse(text = paste0("dataset","$",dD,tD))) %>%
          subset(eval(parse(text = paste0("State_P_",state)))>0)
        
        legend <- factor(eval(parse(text = paste0("DT$", "Agent_", state))))
        
        tempPlot <- qplot(eval(parse(text = paste0("DT$","State_P_",state))), 
                                           geom="histogram", 
                                           fill=legend, 
                                           xlab="Proportion of disturbed area, local scale", 
                                           ylab="Number of surveys") +
                                        theme(axis.text=element_text(size=12), 
                                              axis.title=element_text(size=16,face="bold"),
                                              legend.text=element_text(size=12), 
                                              legend.title=element_text(size=14))
        plotDist[[paste0(dD,tD)]] <- tempPlot
      }
  }
  
  browser()
  
  return(plotDist)
}

plots <- plotDisturbanceSector(dataset = mySim$data, 
                               typeDisturbance = mySim$typeDisturbance[1:2], 
                               disturbanceDimension = mySim$disturbanceDimension)

quickPlot::Plot(plots)
