subsetModels <- function(birdSp, disturbancePredict,
                         prmt,
                         models){

  if(any(grepl(pattern = "focalDistance", names(unlist(prmt))))){
    rw <- which(grepl(pattern = "focalDistance", names(unlist(prmt))))
    focalDistance <- as.numeric(unlist(prmt)[rw])
    if(all(!focalDistance %in% c(100, 500))){
      warning("Predictions won't be performed for focal distances other than 100 or 500")
    }
    if (all(length(unique(focalDistance)) == 1 & focalDistance == 100)){
      comb <- paste0("local", disturbancePredict) 
      } else {
        if (all(length(focalDistance) == 2 & focalDistance == c(100, 500))){ 
          comb <- paste0("neighborhood", disturbancePredict)
          } else {
            NULL
          }
      }
    slct <- which(names(models) %in% comb)
    if (is.null(slct))
      stop("The correct models were not found. The most likely reason:
           the 'glmerBirdModels' is missing from the call")
    models <- models[[slct]]
    whichSp <- match(birdSp, names(models))
    models <- models[whichSp]
  }
  return(models)
}