processOffsets <- function(offsetFilesName = "offsetFiles", envirSim = envir(sim), birdSpecies = sim$birdSpecies){
  
  offsetFiles <- get(x = offsetFilesName, envir = envirSim)
  birdSpecies <- c(birdSpecies, "PKEY")
  
  if (length(offsetFiles) == 1) {
    message(crayon::yellow(paste0("Offset's archive contains only one table, no processing needed. If the .RData file contains more than one object, the biggest one will be returned.")))
    
    listSize <- unlist(lapply(offsetFiles[["newerOffsets"]], FUN = function(x) object.size(x)))
    biggstObj <- which(listSize == max(listSize))
    nameList <- attributes(biggstObj)
    off1 <- offsetFiles[["newerOffsets"]][[nameList$names]] %>%
      .[, ..birdSpecies]
    
    message(crayon::green(paste0("Offsets successfully processed!")))
    return(off1)
    
  } else {
    # PUT THE PKEY OR OTHER INFO
    updatedOffsets <- lapply(X = offsetFiles, FUN = function(dataset){
      listSize <- unlist(lapply(dataset, FUN = function(x) object.size(x)))
      biggstObj <- which(listSize == max(listSize))
      nameList <- attributes(biggstObj)
      rightTable <- nameList$names
      return(dataset[[rightTable]])
    })
    
    whichToChange <- updatedOffsets[["olderOffsets"]]$PKEY[which(updatedOffsets[["olderOffsets"]]$PKEY %in% updatedOffsets[["newerOffsets"]]$PKEY)]
    append <- updatedOffsets[["newerOffsets"]][!PKEY %in% whichToChange,]
    updatedOffsets[["olderOffsets"]][PKEY %in% whichToChange,] <- updatedOffsets[["newerOffsets"]][PKEY %in% whichToChange,]
    DataJoin <- rbind(updatedOffsets[["olderOffsets"]], append)
    off2 <- DataJoin %>%
      .[, ..birdSpecies]
 
    message(crayon::yellow(paste0("Offset's archive contains more than one table, processing tried to keep it updated. If the .RData file contains more than one object, the biggest one will be updated and returned.")))  
    
    message(crayon::green(paste0("Offsets successfully updated!")))
    
  return(off2)
  }
}

