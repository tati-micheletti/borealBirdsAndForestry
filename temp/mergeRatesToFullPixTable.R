# PATCH FOR 500m. THIS IS FIXED FOR FUTURE 100m

#These are the finalPixelTable_****_forSummary_
spatialScale <- 500
lapply(species, function(sp){
  
  fullPixelTable <- readRDS(file.path(folderForTables, paste0("fullPixelTable", sp, paste0(spatialScale, "m"), ".rds")))
  colsToDrop <- names(fullPixelTable)[!names(fullPixelTable) %in% c("pixelID", 
                                                                    grepMulti(x = names(fullPixelTable), 
                                                                            patterns = "rate"))]
  fullPixelTable[, (colsToDrop) := NULL]
  
  lapply(c("finalPixelTable_forSummary_",
           "finalPixTab_BCRPROV_forSummary_",
           "finalPixelTable_Alberta_forSummary_",
           "finalPixelTable_Quebec_forSummary_"), function(tab){
             tabName <- file.path(folderForTables,
                              paste0(tab, sp, spatialScale, "m.rds"))
             tb <- readRDS(tabName)
             merged <- merge(fullPixelTable, tb)
             saveRDS(object = merged, tabName)
             message(crayon::yellow(paste0("Merged 'rates' to ", tab, " for ", sp)))
           })
  message(crayon::green(paste0("Merged 'rates' to all tabs for ", sp)))
})
