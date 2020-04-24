plotYearlyRateOfChange <- function(table, birdSpecies = NULL,
                                   whatToPlot = "abundance", # abundance, cummPerc
                                   namesTabs = "value",
                                   direction = "horizontal"){
library("ggplot2")
if (whatToPlot == "abundance"){
      yLab <- "Habitat supply fluctuation (abundance) \nfrom 1984 to 2011"
}
if (whatToPlot == "cummPerc"){
  yLab <- "Habitat supply fluctuation (percentage) \nfrom 1984 to 2011"
}

if (!is.null(birdSpecies))
    table <- table[species %in% birdSpecies, ]
  p <- ggplot(data = table, aes(x = year,
                                 y = get(whatToPlot), 
                                color = species)) +
    geom_line(size = 1.3) +
    theme_linedraw() +
    labs(y = yLab)
  # if (separatePlots)
  if (!is.null(namesTabs)){
    if (direction == "horizontal")
      p <- p + facet_grid(region ~ ., labeller = labeller(region = namesTabs))
    if (direction == "vertical")
      p <- p + facet_grid(. ~ region, labeller = labeller(region = namesTabs))
  } else {
    if (direction == "horizontal")
      p <- p + facet_grid(region ~ .)
    if (direction == "vertical")
      p <- p + facet_grid(. ~ region)
  }
  return(plot = p)
}
