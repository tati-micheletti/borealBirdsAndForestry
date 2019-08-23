changeInHabitatSupplyThroughTime <- function(fullTableList){
    ratesNames <- usefun::grepMulti(x = names(fullTableList), patterns = "cummRate")
    ratesNames <- c("species", ratesNames)
    rateDT <- fullTableList[, ..ratesNames]
    library("ggplot2")
    # Convert data from wide to long
    rateDT <- melt(rateDT, id.vars = "species")
    # names(rateDT2)[names(rateDT2) == "variable"] <- "year"
    rateDT$year <- as.numeric(usefun::substrBoth(as.character(rateDT$variable), 4, TRUE))
    scipOrig <- getOption("scipen")
    options(scipen=10000)
    on.exit(options(scipen=scipOrig))
    p <- ggplot(data = rateDT, aes(x = year,
                                   y = value*100, color = species)) +
      geom_line(size = 1.3) +
      # scale_x_continuous(labels = scales::comma) +
      labs(y = "Cummulative rate of change in habitat supply \nfrom 1984 to 2011 (in %)")
    return(list(plotEffectsForestry = p, dataDT = rateDT))
  }
