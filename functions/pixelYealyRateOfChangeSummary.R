pixelYealyRateOfChangeSummary <- function(fullTableList, fullTableName, 
                                          subsetName = "value"){
  library("data.table")
  if (file.exists(fullTableName)){
    message(crayon::green(paste0("Summarized operations' table exists. Returning...")))
    rateDT <- readRDS(fullTableName)
  } else {
    fullTableList <- rbindlist(lapply(names(fullTableList), function(BIRD){
      message(crayon::yellow(paste0("Summarized operations' table does not exists. Creating for ", BIRD)))
      t1 <- Sys.time()
      if (is(fullTableList[[BIRD]], "character")){
        tbl <- readRDS(fullTableList[[BIRD]])
      } else {
        tbl <- fullTableList[[BIRD]] 
      }
      names(tbl)[names(tbl) == subsetName] <- "region"
      cols <- c("pixelID", "region", "realAbund0", paste0("realAbund", 1985:2011))
      tbl <- tbl[, ..cols]
      # Rename realAbund0 to realAbund1984
      names(tbl)[names(tbl) == "realAbund0"] <- "realAbund1984"
      sumTable <- tbl[, lapply(.SD, sum, na.rm = TRUE), by = region, 
                      .SDcols = usefun::grepMulti(x = names(tbl), patterns = "realAbund")]
      sumTable$species <- BIRD
      return(sumTable)
    }))
    fullTableList <- melt(fullTableList, id.vars = c("species", "region")) # Convert data from wide to long
    fullTableList[, year := as.numeric(usefun::substrBoth(as.character(variable), 4, TRUE))]
    fullTableList[, variable := NULL]
    fullTableList[, value := round(value, 0)]
    names(fullTableList)[names(fullTableList) == "value"] <- "abundance"
    refAbund <- fullTableList[year == 1984, abundance, by = c("species", "region")]
    names(refAbund)[names(refAbund) == "abundance"] <- "refAbund"
    fullTableList <- merge(fullTableList, refAbund, by = c("species","region"))
    fullTableList[, cummPerc := 100*((abundance-refAbund)/refAbund)]
    fullTableList[, maxLoss := min(cummPerc), by = c("species","region")]
    saveRDS(fullTableList, file = fullTableName)
    return(fullTableList)
  }
}