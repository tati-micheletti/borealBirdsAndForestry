
# This function generated Figure 2 from the manuscript

plotDisturbanceSector <- function(sim = sim, dataset = sim$data, types = sim$typeDisturbance, RColorBrewerPalett = "Pastel1"){
  
  require(ggplot2)
  require(RColorBrewer)
  require(data.table)
  
  # Selecting only the datasets that are of interest
  withNames <- which(grepl(paste(types, collapse = "|"), x = names(dataset)))
  dataset <- dataset[withNames]
  listNames <- colnames(sapply(dataset, names))
  
  # Adding columns to identify the wrap for the graph
  Disturbance <- c(rep("LOCAL SCALE",2),rep("NEIGHBORHOOD SCALE",2))  #Make this automatic based on the name of the dataset
  Type <- rep(c("TRANSITIONAL DISTURBANCES","PERMANENT DISTURBANCES"), times =2) #Make this automatic based on the name of the dataset
  dataset <- Map(cbind, dataset, DISTURBANCE = Disturbance, TYPE = Type)
  
  # Subsetting to values that compose the graph and add column with State_P_X depending on the list Scale
  plotDT <- lapply(X = listNames, FUN = function(x){
    state <- ifelse(grepl("local", x),"100","500")
    DT <- subset(dataset[[x]], eval(parse(text = paste0("State_P_",state)))>0)
    DT$disturbedArea <- eval(parse(text = paste0("DT$","State_P_",state)))
    DT$agentDisturbance <- eval(parse(text = paste0("DT$","Agent_",state)))
    return(DT)})
  names(plotDT) <- listNames
  
  # Comnbine all lists into one dataset
  dataset <- data.table::rbindlist(plotDT, use.names=TRUE)
  
  # Make the graph
  dataset <- as.data.frame(dataset)
  
  graph <- ggplot(dataset, aes(x = disturbedArea, fill=agentDisturbance)) +
    facet_grid(DISTURBANCE ~ TYPE, scales = "free_y") +
    #facet_wrap(DISTURBANCE ~ TYPE, scales = "free_y") +
    geom_histogram(binwidth = 0.05) +
    theme(strip.text.y = element_text(size=12, face="bold"),
          strip.text.x = element_text(size=12, face="bold"),
          legend.position = "right",
          legend.title = element_text(face = "bold"),
          legend.title.align = 0.5,
          axis.text=element_text(size=12),
          axis.title=element_text(size=16,face="bold"),
          legend.text=element_text(size=12),
          panel.background = element_rect(fill = "grey99"),
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    labs(x = "Proportion of disturbed area", y = "Number of surveys") +
    scale_fill_brewer(palette=RColorBrewerPalett, direction = 1, 
                      name = "Disturbance\nAgent")
  
  png(file.path(outputPath(sim),"plotDisturbanceSector.png"), width = 1500, height = 863)
  graph
  dev.off()
  
  return(graph)
}

