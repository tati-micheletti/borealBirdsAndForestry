dataframeBuilding <- function(birdData,
                              birdSpecies
                              ){
  forestListName <- grepMulti(x = names(birdData), patterns = c("Transitional"))
  forestList <- rbindlist(birdData[names(birdData) %in% forestListName])
  birdData <- forestList[!duplicated(forestList)]
  
  # Subsetting to avoid using data from other types of disturbance
  birdDataN <- birdData[Agent_500=="Forestry" | Agent_500==""]
  birdDataFinal <- birdDataN[Agent_100=="Forestry" | Agent_100==""]
  
  #  2. Add age to this "final" dataset. Should do all prep (step 1) in a new function before passing to dataframeBuilding.
  # Get the unique combinations of locations
  birdDataFinal[, XY := paste0(X, ":", Y)]
  uniqueLocations <- data.frame(stringr::str_split_fixed(unique(birdDataFinal$XY), ":", 2))
  names(uniqueLocations) <- c("X", "Y")
  uniqueLocations <- apply(X = uniqueLocations,
                           MARGIN = 2, FUN = as.numeric)
  uniqueLocationsMatrix <- as.matrix(uniqueLocations)
  ageDT <- data.table(X = uniqueLocations[,1], 
                      Y = uniqueLocations[,2])
  birdDataFinal <- merge(birdDataFinal, ageDT, on = c("X", "Y"))
  
  # 4. dataframeBuilding function needs to make a list with copies of all the values of the model 
  # and the specific bird sp offset and log and count (bird names on the cols): each element of 
  # the named list is a data.frame with all info for one bird species
  # What I need from the data.table here:
  # X, Y, BIRD, YYYY, ClusterSP, State_P_100, State_P_500, OFFSET_BIRD, logDENSITY_BIRD, correctedAge
  listOfTables <- lapply(X = birdSpecies, FUN = function(sp){
    birdDependentVars <- grepMulti(x = names(birdDataFinal), patterns = sp)[-2] # Hack for excluding DENSITY (keep logDENSITY)
    IndependentVars <- c("X", "Y", "State_P_100", "State_P_500", "YYYY", "ClusterSP")
    varsToKeep <- c(IndependentVars, birdDependentVars)
    spDT <- birdDataFinal[, ..varsToKeep]
    return(spDT)
  })
  names(listOfTables) <- birdSpecies
  return(listOfTables)
}
