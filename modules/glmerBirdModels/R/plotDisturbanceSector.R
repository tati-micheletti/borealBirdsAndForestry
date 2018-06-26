
# This function generated Figure 2 from the manuscript
# THESE ARE THE MODIFICATIONS ASKED FROM ALBERTO ON 23rd MAY 2018 and 31st May

plotDisturbanceSector <- function(outputPath = outputPath(sim), sim = sim, 
                                  dataset = sim$data, 
                                  types = sim$typeDisturbance, 
                                  RColorBrewerPalett = "Set1"){
  
  require(ggplot2)
  require(RColorBrewer)
  require(data.table)
  
  # Selecting only the datasets that are of interest
  withNames <- which(grepl(paste(types, collapse = "|"), x = names(dataset)))
  dataset <- dataset[withNames]
  listNames <- colnames(sapply(dataset, names))
  
  for (name in names(dataset)){
    dataset[[name]]$DIMENSION <- ifelse(grepl(pattern = "local", x = name),"LOCAL SCALE", 
                                        ifelse(grepl(pattern = "neighborhood", x = name),"NEIGHBORHOOD SCALE",
                                               "LOCAL UNDISTURBED"))
    dataset[[name]]$TYPE <- ifelse(grepl(pattern = "Transitional", x = name),"SUCCESSIONAL DISTURBANCES", 
                                   ifelse(grepl(pattern = "Permanent", x = name),"ALIENATING DISTURBANCES",
                                          "COMBINED DISTURBANCES"))
  }
  
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
  
  # # To include localUndisturbed or Both, comment below # Asked on 31st May
  # datasetNoBoth <- dataset[!dataset$TYPE=="COMBINED DISTURBANCES",]
  # datasetNoBoth <- datasetNoBoth[!datasetNoBoth$DIMENSION=="LOCAL UNDISTURBED",]
  # datasetNoBoth$TYPE <- factor(datasetNoBoth$TYPE, levels = c("SUCCESSIONAL DISTURBANCES","ALIENATING DISTURBANCES"))
  
  graph <- ggplot(dataset, aes(x = disturbedArea, fill=agentDisturbance)) + #Changed dataset #Cganged back datasetNoBoth
    facet_grid(DIMENSION ~ TYPE, scales = "free_y") +
    #facet_wrap(DISTURBANCE ~ TYPE, scales = "free_y") +
    geom_histogram(binwidth = 0.05) +
    theme(strip.text.y = element_text(size=16, face="bold"),
          strip.text.x = element_text(size=16, face="bold"),
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
  
  png(file.path(outputPath,"plotDisturbanceSector.png"), width = 1500, height = 863)
  graph
  dev.off()
  
  return(graph)
}

