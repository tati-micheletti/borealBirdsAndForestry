
reCenterModelData <-function(x, dataset, name){
  
  require(RFmarkerDetector)
  
  dimension <- ifelse(grepl("local", x),"State_P_100","State_P_500")
  
  arrayCols <- as.array(cbind(eval(parse(text = paste0("dataset[[x]]$",dimension))), 
                                                   eval(parse(text = paste0("dataset[[x]]$OF_",name))),
                                                   eval(parse(text = paste0("dataset[[x]]$LOG_BCR_",name)))))
  
  StateCentered <-  RFmarkerDetector::autoscale(arrayCols, exclude = F)
  colnames(StateCentered) <- c("StateNew", paste0("NewOF_",name), paste0("NewLOG_BCR_",name))
  dataset[[x]] <- cbind(dataset[[x]],StateCentered) 
  dataRecentered <- dataset[[x]]
  
    suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ get(dimension) + LOG_BCR_", name, " + ", 
                                                         "offset(NewOF_", name,") + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY)",
                                                         ", family='poisson', data=dataRecentered)")))))
  
  if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
    
    suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ get(dimension) + LOG_BCR_", name, " + ",
                                                           "offset(NewOF_", name,") + (1|ClusterSP) + (1|YYYY)",
                                                           ", family='poisson', data=dataRecentered)")))))
  }
  
  if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
    assign(name,paste(as.character("Bad, bad model. No donut for you! Convergence failed.", 
                                   "Try re-running the model with less random effects."), sep = " "))}
  
  modelsList[[name]] <- get(name)
  models[[x]] <- modelsList

  return(models)
  
  }

