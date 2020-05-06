plotOfLostSpecies <- function(table,
                              namesTabs,
                              direction = "vertical", 
                              plotScatter = FALSE){
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
  
  if (plotScatter){
    table <- dcast(data = table, formula = species + region ~ variable)
    names(table)[names(table) == "proportional abundance"] <- "proportionalAbundance"
    names(table)[names(table) == "proportional loss"] <- "proportionalLoss"
    # Change the point size, and shape
    Require("ggpmisc")
    table$species <- as.factor(table$species)
    dt <- table
    my.formula <- y ~ x
   p <-  ggplot(dt, aes(x = proportionalAbundance, y = proportionalLoss)) +
     geom_point(size = 3, aes(shape = species, color = species)) + 
     scale_shape_manual(values = rep(c(8, 15:18), times = 5)) +
     scale_color_manual(values = rainbow(15)) +
     guides(fill = guide_legend(override.aes = list(linetype = 0)),
            color = guide_legend(override.aes = list(linetype = 0))) +
     geom_smooth(data = table, method = "lm", se = TRUE, color = "black", 
                 formula = my.formula) +
     stat_poly_eq(formula = my.formula,
                  aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                  parse = TRUE) +
     theme(panel.background = element_rect(fill = "lightgrey",
                                           colour = "lightgrey",
                                           size = 0.5, linetype = "solid"))
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
  } else {
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
  }
  return(plot = p)
}