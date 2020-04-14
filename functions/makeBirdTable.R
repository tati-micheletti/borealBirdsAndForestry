makeBirdTable <- function(species = NULL,
                          year = NULL, 
                          onlyNA = FALSE,
                          tableFileName,
                          folderForTables,
                          folderForPredictedRasters,
                          locationReturnBirdAbundanceFUN,
                          typeOfTable = "summarizedTable", 
                          lightLoad = FALSE, 
                          useCache = NULL,
                          tablePerPixel = NULL,
                          spatialScale = 500,
                          rasterToMatch = NULL,
                          overwriteInternals = NULL){ # or "overwrite"; basically Cache argument 'useCache'
  library("future")
  library("future.apply")
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
                                             full.names = TRUE), patterns = c("predicted", 
                                                                              species, spatialScale, 
                                                                              "mYear", y, ".tif"))
      source(locationReturnBirdAbundanceFUN)
      densityTable <- reproducible::Cache(returnBirdAbundance, 
                                          filepath = fl, 
                                          type = y,
                                          fullTableFilename = fullTableFilename, 
                                          summarizedTableFileName = summarizedTableFileName,
                                          whichToLoad = typeOfTable, 
                                          lightLoad = lightLoad, 
                                          onlyNA = onlyNA,
                                          rasterToMatch = rasterToMatch,
                                          cacheRepo = file.path(getwd(), 
                                                                "outputs/posthocAnalysis/cache"),
                                          tablePerPixel = tablePerPixel, 
                                          overwriteInternals = overwriteInternals,
                                          useCache = useCache,
                                          omitArgs = c("useCache", "cacheRepo"))
      densityTable$year <- paste0("year", y)
      return(densityTable)
    }))
    # browser() # Not sure why I had a browser here. Probably forgotten? 08APR20
    dcastedTable <- dcast(data = fullTableList, formula = species + pixelID ~ year, 
                          value.var = "density")
    saveRDS(object = fullTableList, file = bigTableName)
    rm(fullTableList)
    gc()
  }
  return(bigTableName)
}
