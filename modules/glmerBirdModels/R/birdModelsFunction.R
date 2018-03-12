# Bird model functions version 2

birdModelsFunction <- function(combinations, dataset, birdSp){
  
  require(lme4)
  
  models <- list()
  modelsList <- list()
  
  
  models <- lapply(X = combinations, FUN = function(x){
    dimension <- ifelse(grepl("local", x),"State_P_100","State_P_500")
    data <- dataset[[x]]
    
    models <- lapply(X = birdSp, FUN = function(name){
      
      tryCatch({
        
        suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ get(dimension) + LOG_BCR_", name, " + ", 
                                                               "offset(OF_", name,") + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY)",
                                                               ", family='poisson', data=data)")))))
        
        if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
          
          suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ get(dimension) + LOG_BCR_", name, " + ",
                                                                 "offset(OF_", name,") + (1|ClusterSP) + (1|YYYY)",
                                                                 ", family='poisson', data=data)")))))
        }
        
        if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
          assign(name,paste(as.character("Bad, bad model. No donut for you! Convergence failed.", 
                                       "Try re-running the model with less random effects."), sep = " "))
      }
      
      modelsList[[name]] <- get(name)
      models[[x]] <- modelsList
      return(models)
      
    }, error = function(e){
      
      phrase <- c("ERROR: Try rescaling offsets or variables.")
      modelsList[[name]] <- phrase
      models[[x]] <- modelsList
      return(models)
    }
  ) # End tryCatch
      
    })
  
  })
  
  l.models <- list()
  for (i in 1:length(models)){
    l.models[i] <- models[[i]]
  }
  names(l.models) <- combinations
  
  l.models2 <- list()
  for (i in 1:length(l.models)){
    l.models2[i] <- l.models[[i]]
  }
  names(l.models2) <- combinations
  
  return(l.models2)

}
