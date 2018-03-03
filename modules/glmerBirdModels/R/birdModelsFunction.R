birdModelsFunction <- function(sim, disturbanceDimension, typeDisturbance, birdSp){
  
# disturbanceDimension <- c("local", "neighborhood)
# typeDisturbance <- c("Transitional", "Permanent", "Undisturbed", "Both")
#  For undisturbed, it uses neighborhood model, with local==0

  # Still need to update to include a convergence check and an automatic re-run of the models if convergence is not achieved.
  
  require(lme4)
  
  models <- list()
  modelsList <- list()
  
  for (dd in 1:length(disturbanceDimension)){
    for (td in 1:length(typeDisturbance)){
      
      
      if (disturbanceDimension[dd]=="local"){
    dimension <- paste0("State_P_100")
  }
    
  if (disturbanceDimension[dd]=="neighborhood"){
    dimension <- paste0("State_P_500")
  }

    if (typeDisturbance[td]=="Both"){

      data <- sim$data$fullData
  
      }  else
  
      if (disturbanceDimension[dd]=="neighborhood"&typeDisturbance[td]=="Undisturbed"){
        
        next

        } else {
        
      data <- sim$data[[paste0(disturbanceDimension[dd],typeDisturbance[td])]]
      
      }
    
  ##### MODELS #####
      
      for (name in birdSp){
      
        suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ get(dimension) + LOG_BCR_", name, " + ", 
                                              "offset(OF_", name,") + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY)",
                                              ", family='poisson', data=data)")))))
        
      if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){

        suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ get(dimension) + LOG_BCR_", name, " + ",
                                              "offset(OF_", name,") + (1|ClusterSP) + (1|YYYY)",
                                              ", family='poisson', data=data)")))))
      }
        
        if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
          assign(name,paste(as.character("Bad, bad model. No donut for you! Convergence failed. Redo model by hand")))}
          
        modelsList[[name]] <- get(name)
      
      }
      
      models[[paste0(disturbanceDimension[dd],typeDisturbance[td])]] <- modelsList
      
    }
  }
  
  return(models)
}