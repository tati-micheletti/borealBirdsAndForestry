subsetModels <- function(disturbancePredict = sim$disturbancePredict,
                         prmt = sim@params,
                         models = sim$models){
  
  if(any(grepl(pattern = "focalDistance", names(unlist(prmt))))){
    rw <- which(grepl(pattern = "focalDistance", names(unlist(prmt))))
    focalDistance <- as.numeric(unlist(prmt)[rw])
    if(!focalDistance %in% c(100, 500)){
      warning("Predictions won't be performed for focal distances other than 100 or 500")
    }
    comb <- if (focalDistance == 100) paste0("local", disturbancePredict) else 
              if (focalDistance == 500) paste0("neighborhood", disturbancePredict) else NULL
    slct <- which(names(models) %in% comb)
    models <- models[[slct]]
  }
  
  return(models)
}