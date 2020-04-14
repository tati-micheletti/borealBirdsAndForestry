effectsOfForestryWithTime <- function(fullTableList, calculate = TRUE, pathToSave = getwd(),
                                      addError = FALSE, separatePlots = FALSE,
                                      spatialScale, justPlot = FALSE, subsetName = NULL,
                                      subsetValue = NULL){ 
  library("data.table")
  # Use calculate = TRUE when passing pixel table list
  # justPlot = TRUE: fullTableList is the already calculated object
  # subsetName/value = if passing a table that has more than 1 region that is possible to be used for summaries 
  # (i.e. MANGED == 1 UNMANAGED == 2; or BCR or PROV, etc), specify the column name in subsetName (i.e. region, BCR) 
  # and the number in subsetValue (i.e. 1, 8 etc.)
  if (!justPlot){
    plot1Table <- file.path(pathToSave, paste0("plot1Table", subsetName, subsetValue, "_", 
                                               spatialScale,"m.rds"))
    if (calculate){
      if (file.exists(plot1Table)){
        message(crayon::green(paste0("Summarized operations' table exists. Returning...")))
        rateDT <- readRDS(plot1Table)
      } else {
        fullTableList <- rbindlist(lapply(names(fullTableList), function(BIRD){
        t1 <- Sys.time()
        if (is(fullTableList[[BIRD]], "character")){
          tbl <- readRDS(fullTableList[[BIRD]])
        } else {
          tbl <- fullTableList[[BIRD]] 
        }
        tbl <- tbl[get(subsetName) == subsetValue, ] # subsetting for region
        ratesNames <- usefun::grepMulti(x = names(tbl), patterns = "rate")
        tbl <- rbindlist(lapply(X = c("mean", "sd", "min", "max"), FUN = function(op){
          tb <- tbl[, lapply(.SD, get(op), na.rm = TRUE), by = species, .SDcols = ratesNames] 
          tb$operation <- op
          message(crayon::cyan("Operations complete for ", BIRD, "Time elapsed: ", Sys.time()-t1))
          return(tb)
        })
        )
        })
        )
      rateDT <- fullTableList
      rateDT <- melt(rateDT, id.vars = c("species", "operation"))     # Convert data from wide to long
      rateDT <- dcast(data = rateDT, formula = species + variable ~ operation) # Putting operation back as column
      rateDT$year <- as.numeric(usefun::substrBoth(as.character(rateDT$variable), 4, TRUE))
      saveRDS(rateDT, plot1Table)
    }} # If passing the whole summarized table goes straight here
  if (!calculate){
    ratesNames <- usefun::grepMulti(x = names(fullTableList), patterns = "rate")
    ratesNames <- c("species", ratesNames)
    rateDT <- fullTableList[, ..ratesNames]
    rateDT <- melt(rateDT, id.vars = "species")     # Convert data from wide to long
  }
  } else {
  rateDT <- ifelse(is(fullTableList, "character"),
                   readRDS(fullTableList),
                   fullTableList)
}
    library("ggplot2")
  scipOrig <- getOption("scipen")
  options(scipen=10000)
  on.exit(options(scipen=scipOrig))
  p <- ggplot(data = rateDT, aes(x = year,
                                 y = mean, color = species)) +
    geom_line(size = 1.3)
  if (separatePlots)
    p <- p + facet_grid(. ~ species) 
  if (addError)
    p <- p  + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, color = species))
    labs(y = "Rate of change in bird abundance \nin comparison to the previous year")
  return(list(plotEffectsForestry = p, dataDT = rateDT))
}