# dropOutliers

dropOutliers <- function(x, dataset, name, probs){
  
  
  require(stats)
  
  dimension <- ifelse(grepl("local", x),"State_P_100","State_P_500")
  arrayCol <- as.array(eval(parse(text = paste0("dataset[[x]]$OF_",name))))
  
  qnt <- stats::quantile(arrayCol, probs=probs)[1]
  rowsRemove <- which(eval(parse(text = paste0("dataset[[x]]$OF_",name)))<qnt[1] | eval(parse(text = paste0("dataset[[x]]$OF_",name)))>qnt[2])
  newDataset <- dataset[[x]][!rowsRemove]
  
  suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ get(dimension) + LOG_BCR_", name, " + ", 
                                                         "offset(OF_", name,") + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY)",
                                                         ", family='poisson', data=newDataset)")))))
  
  if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
    
    suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ get(dimension) + LOG_BCR_", name, " + ",
                                                           "offset(OF_", name,") + (1|ClusterSP) + (1|YYYY)",
                                                           ", family='poisson', data=newDataset)")))))
  }
  
  if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
    assign(name,paste(as.character("Bad, bad model. No donut for you! Convergence failed.", 
                                   "Try re-running the model with less random effects."), sep = " "))}

  modelsList <- get(name)
  return(modelsList)
  
}
