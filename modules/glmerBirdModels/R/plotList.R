# Make plotList. This will be used for more than one plot.

plotList <- function(dataset = sim$models, combinations = sim$combinations, species = sim$birdSpecies){
  
  require(data.table)
  
  coefTable <- lapply(X = combinations, FUN = function(x){
    
    birdSpecies <- lapply(X = species, FUN = function(name){
      
      isolatedModel <- eval(parse(text = paste0("dataset[[x]]$",name)))
      coef <- as.data.frame(summary(isolatedModel)$coefficients, keep.rownames = TRUE)
      estimate <- coef["get(dimension)",][c(1:2, 4)]
      colnames(estimate) <- c("Estimate", "Std.Error","p")
      rownames(estimate) <- name
      estimate$lowerCI <- estimate$Estimate - estimate$`Std.Error` # Not really CI, actually just std.err +- estimate! 
      estimate$upperCI <- estimate$Estimate + estimate$`Std.Error` # Not really CI, actually just std.err +- estimate!
      
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
  plotTable$Significancy <- ifelse(plotTable$p<0.05,"*","")
  
  return(plotTable)
  
}