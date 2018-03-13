
plotCoefficients <- function(dataset, combinations = sim$combinations, species = sim$birdSpecies){

  require(lmerTest)
  require(data.table)
  require(ggplot2)
  require(ggalt)

  coefTable <- lapply(X = combinations, FUN = function(x){

    birdSpecies <- lapply(X = species, FUN = function(name){
      
      isolatedModel <- eval(parse(text = paste0("dataset[[x]]$",name)))
      coef <- as.data.frame(summary(isolatedModel)$coefficients, keep.rownames = TRUE)
      estimate <- coef["get(dimension)",][c(1:2, 4)]
      colnames(estimate) <- c("Estimate", "Std.Error","p")
      rownames(estimate) <- name
      estimate$lowerCI <- estimate$Estimate - estimate$`Std.Error`
      estimate$upperCI <- estimate$Estimate + estimate$`Std.Error`

      return(estimate)
    })

    birdSpeciesUnlisted <- do.call(rbind, unname(birdSpecies))
    return(birdSpeciesUnlisted)

  })

  names(coefTable) <- combinations

  colToAdd <- lapply(X = combinations, FUN = function(x){

    typeDisturbance <- ifelse(grepl("local", x),"LOCAL",
                              ifelse(grepl("Local", x),"LOCAL UNDISTURBED","NEIGHBORHOOD"))
    disturbanceDimension <- ifelse(grepl("Permanent", x),"PERMANENT",
                                   ifelse(grepl("Transitional", x),"TRANSITIONAL","BOTH"))
    tableCols <- data.frame(typeDisturbance, disturbanceDimension)

    return(tableCols)

  })
  
  plotTable <- Map(cbind, coefTable, colToAdd) %>%
    do.call("rbind", .)
  plotTable$Species <- species
  rownames(plotTable) <- NULL
  plotTable <- plotTable[order(plotTable$Estimate),]

      plot <- ggplot(data = plotTable, aes(y = Species)) +
                    facet_grid(typeDisturbance ~ disturbanceDimension) + #, scales = "free_y"
                    geom_segment(aes(x = lowerCI, xend = upperCI, yend = Species)) +
                    geom_point(aes(x = Estimate), pch=ifelse(plotTable$p < 0.05, 19, 21), 
                                        fill=ifelse(plotTable$p < 0.05, "black", "white")) +
                    theme(strip.text.y = element_text(size=12, face="bold"),
                          strip.text.x = element_text(size=12, face="bold"),
                          legend.title = element_text(face = "bold"),
                          axis.text=element_text(size=12),
                          axis.title=element_text(size=16,face="bold"),
                          panel.background = element_rect(fill = "grey99"),
                          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
                    labs(x = "Abundance estimates", y = "Bird species") +
                  geom_vline(xintercept = 0, linetype="dashed", color="darkgrey")
}