# Observations
#   Not sure negative binomial is the best one… Maybe ZIP would be better? 
#   We can try extra-Poisson dispersion model (ie. 12.3.3. Kery and Schaub 2012), 
#   negative binomial (Kery and Royle 2016) or even regular Poisson (depends really 
#   on the species we are dealing with… I don’t think Poisson is the best for old-forest 
#   associated birds!).
#   
#   We could eventually model time (HH MM) as RE of propability of observation

# Obvious outcomes
#   NN > NL


#   # Assumptions
# 1. Bird density might vary depending on cluster (RE)
# 2. Bird density might vary depending on the year (RE)

dataframeBuilding <- function(birdData,
                              birdSpecies, 
                              ageMap,
                              currentTime,
                              pathData,
                              rP){

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
  

  browser()
  
  # 5. Create the data.frame that I want to populate: - NEXT FUNCTION. 
  # This one should be cached just to be added every year to the one I really want to monitor
  # 
  # Abundance = NA (the one parameter to monitor!),
  # estimate = log of estimate based on LCC_BCR (I have this table somewhere!),
  # age = corrected growth 1985-2011 based off of 2004 layer (and maybe double check the disturbances?! See item 5.),
  # disturbance = values from the focal rasterstack, 
  # X = coordenadas X from "template raster" (one of the mergedFocal?) for all pixels to 
  # forecast (maybe would be smart to crop to only the pixels that are within Sp distribution for computation time?),
  # Y = same as x,
  # offsets = NA
  # 
  # 6. Put the DF in the model and figure out after that...  
  
}



