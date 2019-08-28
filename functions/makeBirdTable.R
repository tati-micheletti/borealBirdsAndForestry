makeBirdTable <- function(species = NULL,
                          year = NULL, 
                          tableFileName,
                          folderForTables,
                          folderForPredictedRasters,
                          locationReturnBirdAbundanceFUN,
                          typeOfTable = "summarizedTable", 
                          lightLoad = FALSE, 
                          tablePerPixel = NULL,
                          overwriteInternals = NULL){ # or "overwrite"; basically Cache argument 'useCache'
  bigTableName <- file.path(folderForTables, paste0(tableFileName, ".rds"))
  if (!file.exists(bigTableName)){
    if (is.null(year))
      year <- 1984:2011
    if (is.null(species))
      species <- "BBWA"
    fullTableList <- rbindlist(lapply(X = year, function(y){
      fullTableFilename <- file.path(folderForTables, 
                                     paste0("fullTable",species, y,".rds"))
      summarizedTableFileName <- file.path(folderForTables,
                                           paste0("birdsTable",species, y,".rds"))
      fl <- usefun::grepMulti(x = list.files(path = folderForPredictedRasters, 
                                             full.names = TRUE), patterns = c("predicted", species, "500mYear", y, ".tif"))
      source(locationReturnBirdAbundanceFUN)
      densityTable <- reproducible::Cache(returnBirdAbundance, filepath = fl, type = y,
                                          fullTableFilename = fullTableFilename, 
                                          summarizedTableFileName = summarizedTableFileName,
                                          whichToLoad = typeOfTable, lightLoad = lightLoad, 
                                          cacheRepo = file.path(getwd(), "outputs/posthocAnalysis/cache"),
                                          tablePerPixel = tablePerPixel, useCache = overwriteInternals,
                                          omitArgs = c("useCache", "cacheRepo"))
      densityTable$year <- paste0("year", y)
      return(densityTable)
    }))
    dcastedTable <- dcast(data = fullTableList, formula = species + pixelID ~ year, value.var = "density")
    saveRDS(object = fullTableList, file = bigTableName)
    rm(fullTableList)
    gc()
  }
  return(bigTableName)
}
