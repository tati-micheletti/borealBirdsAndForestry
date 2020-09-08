divideInChunks <- function(vectorToSplit, 
                           maxPerGroup = NULL,
                           numberOfGroups = NULL){
  if (all(is.null(maxPerGroup), is.null(numberOfGroups))){
    stop("Either maxPerGroup or numberOfGroups needs to be provided")
  }
  if (all(!is.null(maxPerGroup), !is.null(numberOfGroups))){
    stop("Only one of maxPerGroup or numberOfGroups should to be provided")
  }
if (is.null(maxPerGroup)){ # numberOfGroups is known
    maxPerGroup <- ceiling(length(vectorToSplit)/numberOfGroups)
    x <- seq_along(vectorToSplit)
    chunks <- split(vectorToSplit, ceiling(x/maxPerGroup))
    names(chunks) <- paste0("group", 1:length(chunks))
    return(chunks)
}
  if (is.null(numberOfGroups)){ # maxPerGroup is known
    x <- seq_along(vectorToSplit)
    chunks <- split(vectorToSplit, ceiling(x/maxPerGroup))
    names(chunks) <- paste0("group", 1:length(chunks))
    return(chunks)
  }
}
