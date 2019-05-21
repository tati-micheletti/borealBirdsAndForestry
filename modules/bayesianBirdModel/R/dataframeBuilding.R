dataframeBuilding <- function(birdData,
                              birdSpecies, 
                              ageMap
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
  ageValues <- raster::extract(x = ageMap, y = uniqueLocationsMatrix)
  ageDT <- data.table(X = uniqueLocations[,1], 
                      Y = uniqueLocations[,2], 
                      age2004 = ageValues)
  birdDataFinal <- merge(birdDataFinal, ageDT, on = c("X", "Y"))
  
  #  3. Correct age2004 for correct age based on the column YYYY

  birdDataFinal[, correctedAge := (YYYY - 2004) + age2004]
  
  # A few (n = 16) point counts have a mismatch between the age and the disturbance. 
  # This can be due to bad data in a few places, but should be minor, especially because 
  # it is a 2 year mismatch (not too bad). Will correct these specific points to zero.
  birdDataFinal[correctedAge < 0, correctedAge := 0]
  
  # 4. dataframeBuilding function needs to make a list with copies of all the values of the model 
  # and the specific bird sp offset and log and count (bird names on the cols): each element of 
  # the named list is a data.frame with all info for one bird species
  # What I need from the data.table here:
  # X, Y, BIRD, YYYY, ClusterSP, State_P_100, State_P_500, OFFSET_BIRD, logDENSITY_BIRD, correctedAge
  
  listOfTables <- lapply(X = birdSpecies, FUN = function(sp){
    birdDependentVars <- grepMulti(x = names(birdDataFinal), patterns = sp)[-2] # Hack for excluding DENSITY (keep logDENSITY)
    IndependentVars <- c("X", "Y", "State_P_100", "State_P_500", "correctedAge") #"YYYY", "ClusterSP", [ FIX ] TO ADD RE ADD THESE
    varsToKeep <- c(IndependentVars, birdDependentVars)
    spDT <- birdDataFinal[, ..varsToKeep]
    return(spDT)
  })
  names(listOfTables) <- birdSpecies
  
return(listOfTables)
}



