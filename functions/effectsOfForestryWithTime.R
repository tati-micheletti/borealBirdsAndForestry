effectsOfForestryWithTime <- function(fullTableList, calculate = TRUE, pathToSave = getwd(),
                                      addError = FALSE, tableName = NULL,
                                      patternToPlot = "rate", namesTabs = NULL,
                                      spatialScale, subsetName = NULL, 
                                      birdSpecies = NULL, yLab = NULL, 
                                      whichPlot = "p1", whichOp = "mean"){ 
  library("data.table")
  # Use calculate = TRUE when passing pixel table list
  # namesTabs = names for the horizontal facet tabs. Needs to be a list in the form:
  # namesTabs <- list(
  #   'Hospital#1'="Some Hospital",
  #   'Hospital#2'="Another Hospital",
  #   'Hospital#3'="Hospital Number 3",
  #   'Hospital#4'="The Other Hospital"
  # )
  # subsetName = if passing a table that has more than 1 region what is the name of the column that contains these? 
  # (i.e. MANGED == 1 UNMANAGED == 2; or BCR or PROV, etc), specify the column name in subsetName (i.e. region, BCR) 
  # and the number in subsetValue (i.e. 1, 8 etc.)
  # oneSpecies : If we want the plot for only one species. However, this is not very efficient as it loads the 
  # whole table to subset one species
  
    plot1Table <- file.path(pathToSave, paste0(whichPlot, "Table", tableName, "_", 
                                               spatialScale,"m.rds"))
    if (calculate){
      if (file.exists(plot1Table)){
        message(crayon::green(paste0("Summarized operations' table exists. Returning...")))
        rateDT <- readRDS(plot1Table)
      } else {
        message(crayon::yellow(paste0("Summarized operations' table does not exists. Creating...")))
        fullTableList <- rbindlist(lapply(names(fullTableList), function(BIRD){
          t1 <- Sys.time()
          if (is(fullTableList[[BIRD]], "character")){
            tbl <- readRDS(fullTableList[[BIRD]])
          } else {
            tbl <- fullTableList[[BIRD]] 
          }
          names(tbl)[names(tbl) == subsetName] <- "region"
          subsetName <- "region"
          colsToUseNames <- usefun::grepMulti(x = names(tbl), patterns = patternToPlot) # Maybe this colToUse (i.e. 'rate') should be a param
          tbl <- rbindlist(lapply(X = c("mean", "sd", "min", "max"), FUN = function(op){
            tb <- tbl[, lapply(.SD, get(op), na.rm = TRUE), by = subsetName, .SDcols = colsToUseNames] 
            tb$operation <- op
            return(tb)
          }))
          message(crayon::cyan("Operations complete for ", BIRD, "Time elapsed: ", Sys.time()-t1))
          tbl$species <- BIRD
          return(tbl)
        })
        )
        rateDT <- fullTableList
        rateDT <- melt(rateDT, id.vars = c("species", "operation", "region")) # Convert data from wide to long
        rateDT <- dcast(data = rateDT, formula = species + region + variable ~ operation) # Putting operation back as column
        rateDT$year <- as.numeric(usefun::substrBoth(as.character(rateDT$variable), 4, TRUE))
        saveRDS(rateDT, plot1Table)
      }} # If passing the whole summarized table goes straight here
  if (!calculate){ # POTENTIALLY PROBLEMATIC AFTER IMPLEMENTING REGIONS [20APRIL20]
    browser() # Debug to make sure it works
    colsToUseNames <- usefun::grepMulti(x = names(fullTableList), patterns = patternToPlot)
    colsToUseNames <- c("species", colsToUseNames)
    rateDT <- fullTableList[, ..colsToUseNames]
    rateDT <- melt(rateDT, id.vars = "species")     # Convert data from wide to long
  }
    rateDT$region <- as.character(rateDT$region)
    # separatePlots <- ifelse(length(unique(rateDT$region)) == 1,
    #                         FALSE, TRUE)
    library("ggplot2")
    if (all(is.null(yLab), whichPlot == "p1")){
      yLab <- "Rate of change in bird abundance \nin comparison to the previous year"
    } else {
      if (all(is.null(yLab), whichPlot == "p2"))
      yLab <- "Cummulative rate of change in habitat supply \nfrom 1984 to 2011"
    }
  if (!is.null(birdSpecies))
    rateDT <- rateDT[species %in% birdSpecies, ]
  p <- ggplot(data = rateDT, aes(x = year,
                                 y = get(whichOp), color = species)) +
    geom_line(size = 1.3) +
    theme_linedraw() +
    labs(y = yLab)
  # if (separatePlots)
    if (!is.null(namesTabs)){
      p <- p + facet_grid(region ~ ., labeller = labeller(region = namesTabs))
    } else {
      p <- p + facet_grid(region ~ .)
    }
  if (addError)
    p <- p  + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd, color = species))
  return(list(plotEffectsForestry = p, dataDT = rateDT))
}