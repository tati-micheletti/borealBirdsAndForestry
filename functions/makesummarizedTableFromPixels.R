makesummarizedTableFromPixels <- function(tabName, 
                                          finalPixelTableList, 
                                          foldWhereToSave, 
                                          column, 
                                          species){
  
  summarizedTableFromPixelsPath <- file.path(foldWhereToSave, 
                                             tabName)
  if (!file.exists(summarizedTableFromPixelsPath)){
    summarizedTable <- rbindlist(lapply(species, function(BIRD){
      birdTable <- readRDS(finalPixelTableList[[BIRD]])
      birdTable <- na.omit(birdTable) #TODO ugly shortcut for the presentation! Why do I still have NA's in this table? -- these are until 2005...
      # 23NOV19 --> I still have 605 pixels that are NA for Abund. don't know where this is coming from.... Making a temporary fix --> PROBABLY THE POTENTIAL PROBLEM MENTIONED IN potentialProblem.R
      tableSummaryByRegion <- rbindlist(lapply(unique(birdTable[[column]])[!is.na(unique(birdTable[[column]]))], 
                                               function(reg){
                                                 if (doAssertions){# Make assertions to make sure we have the same amount of pixels with info
                                                   ok1 <- sum(!is.na(birdTable$minrealAbund1985)) == NROW(birdTable)
                                                   ok2 <- sum(!is.na(birdTable$maxrealAbund1985)) == NROW(birdTable)
                                                   ok3 <- sum(!is.na(birdTable$minrealAbund2011)) == NROW(birdTable)
                                                   ok4 <- sum(!is.na(birdTable$maxrealAbund2011)) == NROW(birdTable)
                                                   birdTable <- na.omit(birdTable, cols = "Abund")
                                                   if(!all(ok1, ok2, ok3, ok4)){
                                                     message("There are still NA's in the birdTable. Debug")
                                                   } 
                                                 }
      abund0 <- birdTable[get(column) == reg, sum(realAbund0)]
      abund1985 <- birdTable[get(column) == reg, sum(realAbund1985)]
      minAbund1985 <- birdTable[get(column) == reg, sum(minrealAbund1985)]
      maxAbund1985 <- birdTable[get(column) == reg, sum(maxrealAbund1985)]
      abund2011 <- birdTable[get(column) == reg, sum(realAbund2011)]
      diff2011_0 <- abund2011-abund0
      diff2011_1985min <- abund2011-minAbund1985
      diff2011_1985max <- abund2011-maxAbund1985
      diff2011_1985exp <- abund2011-abund1985
      summarizedTable <- data.table::data.table(species = BIRD,
                                                region = reg, # Each region/polygon of the shapefile
                                                abund0 = abund0,
                                                abund1985 = abund1985,
      minAbund1985 = minAbund1985,
      maxAbund1985 = maxAbund1985,
      abund2011 = abund2011,
      diff2011_1985min = diff2011_1985min,
      diff2011_1985max = diff2011_1985max,
      diff2011_1985exp = diff2011_1985exp,
      diff2011_0 = diff2011_0,
      range = NROW(birdTable),
      diffPerYear0 = diff2011_0/27, # We have a time series of 27 years
      diffPerYearMin = diff2011_1985min/27,
      diffPerYearMax = diff2011_1985max/27,
      diffPerYearExp = diff2011_1985exp/27,
      propDiff0 = (abund2011-abund0)/abund0,
      propDiffExp = (abund2011-abund1985)/abund1985,
      propDiffMin1985 = (abund2011-minAbund1985)/minAbund1985,
      propDiffMax1985 = (abund2011-maxAbund1985)/maxAbund1985)
      return(summarizedTable)
  }))
      rm(birdTable); gc()
      message(crayon::white("Summarized table finished for ", BIRD))
      return(tableSummaryByRegion)
    })
    )
    saveRDS(object = summarizedTable, file = summarizedTableFromPixelsPath)
  } else{
  summarizedTable <- readRDS(summarizedTableFromPixelsPath)
  }
  return(summarizedTable)
}