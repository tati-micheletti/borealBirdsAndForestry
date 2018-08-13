# Bird model functions version 3 - TO be used with both Alberto's data and updated versions of offsets and densities: TO BE USED WHEN PREDICTING

birdModelsFunction <- function(combinations = sim$combinations,
                               birdSp = sim$birdSpecies,
                               dataset = sim$data){
  require(lme4)
  
  models <- list()
  modelsList <- list()
  errorModels <- list()
  
  models <- lapply(X = combinations, FUN = function(x){
    if (grepl("local", x)) {
      data <- dataset[[x]]
      birds <- lapply(X = birdSp, FUN = function(name){
        
        tryCatch({
          message(crayon::yellow(paste0("Running ", x," models for ", name)))
          suppressMessages(assign(name, eval(parse(text = paste0("glmer(", name, " ~ State_P_100 + logDENSITY_", name, " + ",
                                                                 "(1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), ",
                                                                 "offset = OFFSET_", name,
                                                                 ", family = 'poisson', data = data)")))))
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(", name, " ~ State_P_100 + logDENSITY_", name, " + ",
                                                                   "(1|ClusterSP) + (1|YYYY), ",
                                                                   "offset = OFFSET_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(", name, " ~ State_P_100 + logDENSITY_", name, " + ",
                                                                   "(1|ClusterSP), ",
                                                                   "offset = OFFSET_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glm(", name, " ~ State_P_100 + logDENSITY_", name, ", ",
                                                                   "offset = OFFSET_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          modelsList <- get(name)
          return(modelsList)
          
        }, error = function(e){
          
          assign(name, paste(as.character("Bad, bad model. No donut for you! Convergence failed.")))
          
          modelsList <- get(name)
          return(modelsList)
        }
        ) # End tryCatch
        
      })
    } else {
      data <- dataset[[x]]
      birds <- lapply(X = birdSp, FUN = function(name){
        
        tryCatch({
          message(crayon::yellow(paste0("Running ", x," models for ", name)))
          suppressMessages(assign(name, eval(parse(text = paste0("glmer(", name, " ~ State_P_500 + logDENSITY_", name, " + ",
                                                                 "(1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), ",
                                                                 "offset = OFFSET_", name,
                                                                 ", family = 'poisson', data = data)")))))
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(", name, " ~ State_P_500 + logDENSITY_", name, " + ",
                                                                   "(1|ClusterSP) + (1|YYYY), ",
                                                                   "offset = OFFSET_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glmer(", name, " ~ State_P_500 + logDENSITY_", name, " + ",
                                                                   "(1|ClusterSP), ",
                                                                   "offset = OFFSET_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          if (!is.null(eval(parse(text = paste0(name,'@optinfo$conv$lme4$messages'))))){
            
            suppressMessages(assign(name, eval(parse(text = paste0("glm(", name, " ~ State_P_500 + logDENSITY_", name, ", ",
                                                                   "offset = OFFSET_", name,
                                                                   ", family = 'poisson', data = data)")))))
          }
          
          modelsList <- get(name)
          return(modelsList)
          
        }, error = function(e){
          
          assign(name, paste(as.character("Bad, bad model. No donut for you! Convergence failed.")))
          
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