createModel <- function(bird, data, useRE){
if (useRE) {tryCatch({
  message(crayon::yellow(paste0("Fitting models for ", bird)))
  suppressMessages(assign(bird, eval(parse(text = paste0("glmer(", bird, " ~ State_P_500 + logDENSITY_", bird, " + ",
                                                         "(1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), ",
                                                         "offset = OFFSET_", bird,
                                                         ", family = 'poisson', data = data)")))))
  
  if (!is.null(eval(parse(text = paste0(bird,'@optinfo$conv$lme4$messages'))))){
    
    suppressMessages(assign(bird, eval(parse(text = paste0("glmer(", bird, " ~ State_P_500 + logDENSITY_", bird, " + ",
                                                           "(1|ClusterSP) + (1|YYYY), ",
                                                           "offset = OFFSET_", bird,
                                                           ", family = 'poisson', data = data)")))))
  }
  
  if (!is.null(eval(parse(text = paste0(bird,'@optinfo$conv$lme4$messages'))))){
    
    suppressMessages(assign(bird, eval(parse(text = paste0("glmer(", bird, " ~ State_P_500 + logDENSITY_", bird, " + ",
                                                           "(1|ClusterSP), ",
                                                           "offset = OFFSET_", bird,
                                                           ", family = 'poisson', data = data)")))))
  }
  
  if (!is.null(eval(parse(text = paste0(bird,'@optinfo$conv$lme4$messages'))))){
    
    suppressMessages(assign(bird, eval(parse(text = paste0("glm(", bird, " ~ State_P_500 + logDENSITY_", bird, ", ",
                                                           "offset = OFFSET_", bird,
                                                           ", family = 'poisson', data = data)")))))
  }
  
  modelsList <- get(bird)
  return(modelsList)
  
}, error = function(e){
  
  assign(bird, paste(as.character("Bad, bad model. No donut for you! Convergence failed.")))
  
  modelsList <- get(bird)
  return(modelsList)
}
) # End tryCatch
} else {
  suppressMessages(assign(bird, eval(parse(text = paste0("glm(", bird, " ~ State_P_500 + logDENSITY_", bird, ", ",
                                                         "offset = OFFSET_", bird,
                                                         ", family = 'poisson', data = data)")))))
  modelsList <- get(bird)
  return(modelsList)
  
  }
}