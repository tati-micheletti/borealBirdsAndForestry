# Make plotList. This will be used for more than one plot.

plotList <- function(dataset = sim$models, combinations = sim$combinations, birdSp = sim$birdSpecies){
  
  require(data.table)
  
  coefTable <- lapply(X = combinations, FUN = function(x){
    
    birdSpecies <- lapply(X = birdSp, FUN = function(name){
      
      isolatedModel <- eval(parse(text = paste0("dataset[[x]]$",name)))
      coef <- as.data.frame(base::summary(isolatedModel)$coefficients, keep.rownames = TRUE)
      estimate <- coef["get(dimension)",][c(1:2, 4)]
      colnames(estimate) <- c("Estimate", "Std.Error","p")
      rownames(estimate) <- name
      
      # JUSTIFICATION FOR USING St.Error*1.96 (errors should be close to normal in a high degree of freedom model) - is this my case?
      # The t-distribution is asymptotically normal and the degrees of freedom for the error 
      # term in many multi-level designs is so high that the error distribution is normal at 
      # that point. Therefore, if you have a design with lots of degrees of freedom this is 
      # a perfectly reasonable confidence interval estimate.
      
      estimate$lowerCI <- estimate$Estimate - 1.96*estimate$`Std.Error` # Not really CI, actually just std.err +- estimate! 
      estimate$upperCI <- estimate$Estimate + 1.96*estimate$`Std.Error` # Not really CI, actually just std.err +- estimate!
      
      return(estimate)
    })
    
    birdSpeciesUnlisted <- do.call(rbind, unname(birdSpecies))
    return(birdSpeciesUnlisted)
    
  })
  
  names(coefTable) <- combinations
  
  colToAdd <- lapply(X = combinations, FUN = function(x){
    
    disturbanceDimension <- ifelse(grepl("local", x),"LOCAL",
                              ifelse(grepl("Local", x),"LOCAL UNDISTURBED","NEIGHBORHOOD"))
    typeDisturbance <- ifelse(grepl("Permanent", x),"PERMANENT",
                                   ifelse(grepl("Transitional", x),"TRANSITIONAL","BOTH"))
    tableCols <- data.frame(typeDisturbance, disturbanceDimension)
    
    return(tableCols)
    
  })
  
    plotTable <- Map(cbind, coefTable, colToAdd) %>%
    do.call("rbind", .)
  plotTable$Species <- birdSp
  rownames(plotTable) <- NULL
  plotTable <- plotTable[order(plotTable$Estimate),]
  plotTable$Significancy <- ifelse(plotTable$p<0.1,"YES","NO")
  
  write.csv(mySimOut$plotList, file = file.path(paths$outputPath, "plotList.csv"))
  
  return(plotTable)
  
}