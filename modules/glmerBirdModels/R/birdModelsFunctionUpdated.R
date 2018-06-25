# Bird model functions version 2 - TO BE USED WHEN PREDICTING

birdModelsFunctionUpdated <- function(combinations = sim$combinations, dataset = sim$data, birdSp = sim$birdSpecies){
  
  require(lme4)
  
  models <- list()
  modelsList <- list()
  errorModels <- list()
  
  models <- lapply(X = combinations, FUN = function(x){
    if (grepl("local", x)) {
    data <- dataset[[x]]
    birds <- lapply(X = birdSp, FUN = function(name){
      
      tryCatch({

        suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_100 + LOG_BCR_", name, " + ",
                                                               "(1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), ",
                                                               "offset = OF_", name,
                                                               ", family = 'poisson', data = data)")))))
        
        if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
          
          suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_100 + LOG_BCR_", name, " + ",
                                                                 "(1|ClusterSP) + (1|YYYY), ",
                                                                 "offset = OF_", name,
                                                                 ", family = 'poisson', data = data)")))))
        }
        
        if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
          
          suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_100 + LOG_BCR_", name, " + ",
                                                                 "(1|ClusterSP), ",
                                                                 "offset = OF_", name,
                                                                 ", family = 'poisson', data = data)")))))
        }
        
        if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
          
          suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_100 + LOG_BCR_", name, " + ",
                                                                 "offset = OF_", name,
                                                                 ", family = 'poisson', data = data)")))))
        }
        
        if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
          assign(name,paste(as.character("Bad, bad model. No donut for you! Convergence failed.", 
                                         "Try re-running the model with less random effects."), sep = " "))
        }
        
        modelsList <- get(name)
        return(modelsList)

        }, error = function(e){
          
          suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_100 + LOG_BCR_", name, " + ", 
                                                                 "(1|YYYY) + (1|ClusterSP:YYYY), ",
                                                                 "offset = OF_", name,
                                                                 ", family = 'poisson', data = data)")))))
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_100 + LOG_BCR_", name, " + ",
                                                                   "(1|ClusterSP) + (1|YYYY), ",
                                                                   "offset = OF_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_100 + LOG_BCR_", name, " + ",
                                                                   "(1|ClusterSP), ",
                                                                   "offset = OF_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_100 + LOG_BCR_", name, " + ",
                                                                   "offset = OF_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            assign(name,paste(as.character("Bad, bad model. No donut for you! Convergence failed.", 
                                           "Try re-running the model with less random effects."), sep = " "))
         
          }
            
            modelsList <- get(name)
            return(modelsList)
        
      }
      ) # End tryCatch
      
     })
    } else {
      data <- dataset[[x]]
      birds <- lapply(X = birdSp, FUN = function(name){
        
        tryCatch({
          
          suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_500 + LOG_BCR_", name, " + ", 
                                                                 "(1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), ",
                                                                 "offset = OF_", name,
                                                                 ", family='poisson', data=data)")))))
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_500 + LOG_BCR_", name, " + ",
                                                                   "(1|ClusterSP) + (1|YYYY), ",
                                                                   "offset = OF_", name,
                                                                   ", family='poisson', data=data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_500 + LOG_BCR_", name, " + ",
                                                                   "(1|ClusterSP), ",
                                                                   "offset = OF_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_500 + LOG_BCR_", name, " + ",
                                                                   "offset = OF_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            assign(name,paste(as.character("Bad, bad model. No donut for you! Convergence failed.", 
                                           "Try re-running the model with less random effects."), sep = " "))
          }
          
          modelsList <- get(name)
          return(modelsList)
          
        }, error = function(e){
          
          suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_500 + LOG_BCR_", name, " + ", 
                                                                 "(1|YYYY) + (1|ClusterSP:YYYY), ",
                                                                 "offset = OF_", name,
                                                                 ", family='poisson', data=data)")))))
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_500 + LOG_BCR_", name, " + ",
                                                                   "(1|ClusterSP) + (1|YYYY), ",
                                                                   "offset = OF_", name,
                                                                   ", family='poisson', data=data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_500 + LOG_BCR_", name, " + ",
                                                                   "(1|ClusterSP), ",
                                                                   "offset = OF_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(AB_", name, " ~ State_P_500 + LOG_BCR_", name, " + ",
                                                                   "offset = OF_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            assign(name,paste(as.character("Bad, bad model. No donut for you! Convergence failed.", 
                                           "Try re-running the model with less random effects."), sep = " "))
            
          }
          
          modelsList <- get(name)
          return(modelsList)
          
        }
        ) # End tryCatch
        
      })
    }
    
    names(birds) <- birdSp
    return(birds)
  })
  
  names(models) <- combinations
  return(models)
  
}
