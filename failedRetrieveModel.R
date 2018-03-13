# All failed attempts on retrieving models with names from lapply in birdModelFunction

l.models <- list()
mod1 <- models[[1]]
l.models[["localTransitional"]] <- mod1
l.models[["localTransitional"]][["BBWA"]] <- l.models$localTransitional[[1]]
l.models[["localTransitional"]][["CONW"]] <- l.models$localTransitional[[2]]

models2 <- lapply(X = models, FUN = function(x){
  tempList <- list()
  tempList <- x[[1]]
  names(tempList) <- names(x[[1]])
  return(tempList)
})

models3 <- lapply(X = models, FUN = function(x){
  tempList <- list()
  tempList <- x[[2]]
  names(tempList) <- names(x[[2]])
  return(tempList)
})

l <- list(models2, models3)

keys <- c("localTransitional","neighborhoodTransitional","LocalUndisturbedTransitional",
          "localPermanent","neighborhoodPermanent","LocalUndisturbedPermanent",
          "localBoth","neighborhoodBoth","LocalUndisturbedBoth")

models4 <- setNames(do.call(mapply, c(FUN=c, lapply(l, `[`, keys))), keys)


names(models[[1]][[1]])
