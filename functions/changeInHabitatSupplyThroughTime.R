changeInHabitatSupplyThroughTime <- function(fullTableList, calculate, pathToSave = getwd(),
                                             addError = FALSE, separatePlots = FALSE){ # Use calculate = TRUE when passing pixel table list
  if (calculate){
    summarizedOPTablePath <- file.path(pathToSave, "summarizedOPTableCum.rds")
    if (file.exists(summarizedOPTablePath)){
      message(crayon::green(paste0("Summarized operations' table exist for all birds. Returning...")))
      rateDT <- readRDS(summarizedOPTablePath)
    } else {
      fullTableList <- rbindlist(lapply(names(fullTableList), function(BIRD){
        tbl <- readRDS(fullTableList[[BIRD]])
        tbl <- na.omit(tbl)
        ratesNames <- usefun::grepMulti(x = names(tbl), patterns = "cummRate")
        tbl <- rbindlist(lapply(X = c("mean", "sd", "min", "max"), FUN = function(op){
          tb <- tbl[, lapply(.SD, get(op), na.rm = TRUE), by = species, .SDcols = ratesNames] 
          tb$operation <- op
          message(crayon::cyan("Operations complete for ", BIRD))
          return(tb)
        })
        )
      })
      )    
      rateDT <- fullTableList
      rateDT <- melt(rateDT, id.vars = c("species", "operation"))     # Convert data from wide to long
      rateDT <- dcast(data = rateDT, formula = species + variable ~ operation) # Putting operation back as column
      rateDT$year <- as.numeric(usefun::substrBoth(as.character(rateDT$variable), 4, TRUE))
      saveRDS(rateDT, summarizedOPTablePath)
    }} # If passing the whole summarized table goes straight here
  if (!calculate){
    ratesNames <- usefun::grepMulti(x = names(fullTableList), patterns = "cummRate")
    ratesNames <- c("species", ratesNames)
    rateDT <- fullTableList[, ..ratesNames]
    rateDT <- melt(rateDT, id.vars = "species")     # Convert data from wide to long
  }
  library("ggplot2")
  scipOrig <- getOption("scipen")
  options(scipen=10000)
  on.exit(options(scipen=scipOrig))
  p <- ggplot(data = rateDT, aes(x = year,
                                 y = mean, color = species)) +
    labs(y = "Cummulative rate of change in habitat supply \nfrom 1984 to 2011") +
    geom_line(size = 1.3)
    if (separatePlots)
    p <- p + facet_grid(. ~ species) 
    if (addError)
    p <- p  + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, color = species))
  return(list(plotEffectsForestry = p, dataDT = rateDT))
}