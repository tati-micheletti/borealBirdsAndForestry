retrieveOffsets <- function(urlOffsets = sim$urlOffsets, 
                            pathData = dataPath(sim), 
                            newestDataset = sim$newestDataset,
                            olderDataset = sim$olderDataset){

   newerOffsets <- prepInputs(url = urlOffsets,
                             targetFile = newestDataset,
                             destinationPath = pathData, fun = "load")

   newerOffsets <- lapply(X = newerOffsets, FUN = function(x) {
     x <- as.data.frame(x)
     x$PKEY <- rownames(x)
     x <- data.table::data.table(x)
     return(x)
     })
  
   tryCatch({
     olderOffsets <- prepInputs(url = urlOffsets,
                 targetFile = olderDataset,
                 destinationPath = pathData, fun = "load")
     olderOffsets <- lapply(X = olderOffsets, FUN = function(x) {
       x <- as.data.frame(x)
       x$PKEY <- rownames(x)
       x <- data.table::data.table(x)
       return(x)
     })
     }
     , error = function(e) {
                   message(crayon::yellow(paste0("Only the newest dataset was retrieved. OlderDataset does not exist or was misspecified")))
       })
   
   message(crayon::green(paste0("Offsets successfully retrieved!")))
  
  if (exists("olderOffsets")){
    return(list(olderOffsets = olderOffsets, newerOffsets = newerOffsets))    
  } else {
    return(list(newerOffsets = newerOffsets))
  }

}

