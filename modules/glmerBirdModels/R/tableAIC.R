# Extracting Table S1: tableS1-1 considers LOCAL UNDISTURBED, while tableS1-2 doesn't  

tableAIC <- function(outputPath = outputPath(sim), sim = sim, models = sim$models, birdSp = sim$birdSpecies, combinations = sim$combinations){

  require(reshape2)
  require(tibble)
  require(reproducible)
  
  tableForAIC <- lapply(X = combinations, FUN = function(x){
    birds <- lapply(X = birdSp, FUN = function(bird){
      
      disturbanceDimension <- ifelse(grepl("local", x),"LOCAL",
                                ifelse(grepl("Local", x),"LOCAL_UNDISTURBED","NEIGHBORHOOD"))
      typeDisturbance <- ifelse(grepl("Permanent", x),"PERMANENT",
                                     ifelse(grepl("Transitional", x),"TRANSITIONAL","BOTH"))
      isolatedModel <- eval(parse(text = paste0("models[[x]]$",bird)))
      aic <- data.frame(Species = bird, 
                        TypeDisturbance = typeDisturbance, 
                        DisturbanceDimension = disturbanceDimension,
                        AIC = AIC(isolatedModel))
      
      return(aic)
    })
    
    names(birds) <- birdSp
    return(birds)
    
  })
  
  names(tableForAIC) <- combinations
  list <- unlist(tableForAIC, recursive = FALSE)
  tableAIC <- do.call("rbind", unname(list))
  tableAIC$Cols <- paste(tableAIC$DisturbanceDimension,tableAIC$TypeDisturbance, sep = "_")
  tableAIC <- tableAIC[,-c(2:3)]
  finalTable <- reshape2::dcast(tableAIC, Species ~ Cols, value.var="AIC")
  colNames <- colnames(finalTable)[-1]

  for (nameColumn in colNames){
    finalTable <- eval(parse(text = paste0("add_column(.data = finalTable, d.", nameColumn, " = NA, .after = c(nameColumn))")))
  }

  # WITH UNDISTURBED
  partTableAll <- finalTable[,-1]
  colsDelta <- grepl(pattern = "d.", colnames(partTableAll))
  toAddValue <- which(colsDelta)
  
  for (i in 1:nrow(partTableAll)){
    j <-  which.min(partTableAll[i,])
    partTableAll[i,j+1] <- 0
    refVal <- partTableAll[i,j]
    for (t in 1:ncol(partTableAll)){
      ifelse(!is.na(partTableAll[i, t]),
             next,
             {partTableAll[i, t] <- partTableAll[i, t-1]-refVal})
    }
  }

  tableS1.withUndis <- cbind(Species = finalTable[,1], partTableAll)
  write.csv(tableS1.withUndis, file.path(outputPath, "TableS1-1.csv"))
  
  # WITHOUT UNDISTURBED

  colsKeep <- !(grepl(pattern = "UNDISTURBED", colnames(finalTable)))
  partTableUndis <- finalTable[colsKeep] %>%
    .[,-1]
  tabUndist <- finalTable[,!colsKeep]
  tabUndist <- tabUndist[,!grepl(pattern = "d.", colnames(tabUndist))]
  
  colsDelta <- grepl(pattern = "d.", colnames(partTableUndis))
  toAddValue <- which(colsDelta)
  
  for (i in 1:nrow(partTableUndis)){
    j <-  which.min(partTableUndis[i,])
    partTableUndis[i,j+1] <- 0
    refVal <- partTableUndis[i,j]
    for (t in 1:ncol(partTableUndis)){
      ifelse(!is.na(partTableUndis[i, t]),
             next,
             {partTableUndis[i, t] <- partTableUndis[i, t-1]-refVal})
    }
  }
  
  tableS1.withoutUndis <- cbind(Species = finalTable[,1], partTableUndis, tabUndist)
  write.csv(tableS1.withUndis, file.path(outputPath, "TableS1-2.csv"))
  
  return(tableS1.withoutUndis)
}