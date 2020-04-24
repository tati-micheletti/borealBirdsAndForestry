plotOfLostSpecies <- function(table,
                              namesTabs,
                              direction = "vertical"){
  # Not doing this per year! Just total
  table <- table[, c("species", "region", "refAbund", "maxLoss")]
  table <- unique(table)
  table[, maxLoss := -1*maxLoss]
  table[, refAbundSum := sum(refAbund, na.rm = TRUE), by = "region"]
  table[, maxLossSum := sum(maxLoss, na.rm = TRUE), by = "region"]
  table[, c("proportional abundance", "proportional loss") := list(refAbund/refAbundSum, maxLoss/maxLossSum)]
  table[, c("refAbund", "maxLoss", "refAbundSum", "maxLossSum") := NULL]
  table <- melt(data = table, id.vars = c("species", "region"))
  for (j in seq_len(ncol(table)))
    set(table,which(is.na(table[[j]])),j,0)
  library("ggplot2")
  p <- ggplot(table, aes(fill = species, y = value, x = variable)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_linedraw()
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