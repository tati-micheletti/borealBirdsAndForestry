# Extracting Table S1

tableAIC <- function(sim = sim, models = sim$models, speciesList = sim$birdSpecies, combinations = sim$combinations){

  require(reshape2)
  require(tibble)
  require(reproducible)
  
  tableForAIC <- lapply(X = combinations, FUN = function(x){
    birds <- lapply(X = birdSpecies, FUN = function(bird){
      
      typeDisturbance <- ifelse(grepl("local", x),"LOCAL",
                                ifelse(grepl("Local", x),"LOCAL UNDISTURBED","NEIGHBORHOOD"))
      disturbanceDimension <- ifelse(grepl("Permanent", x),"PERMANENT",
                                     ifelse(grepl("Transitional", x),"TRANSITIONAL","BOTH"))
      
      isolatedModel <- eval(parse(text = paste0("models[[x]]$",bird)))
      aic <- data.frame(Species = bird, 
                        TypeDisturbance = typeDisturbance, 
                        DisturbanceDimension = disturbanceDimension,
                        AIC = AIC(isolatedModel))
      return(aic)
    })
    
    names(birds) <- birdSpecies
    return(birds)
    
  })
  
  names(tableForAIC) <- combinations
  
  list <- unlist(tableForAIC, recursive = FALSE)
  tableAIC <- do.call("rbind", unname(list))
  tableAIC$Cols <- paste(tableAIC$TypeDisturbance, tableAIC$DisturbanceDimension, sep = " ")
  tableAIC <- tableAIC[,-c(2:3)]
  finalTable <- reshape2::dcast(tableAIC, Species ~ Cols, value.var="AIC")
  delta <- numeric(length = length(models))
  for (i in 1:length(models)){
  delta[i] <- c(paste("DeltaAIC",i, sep = " "))}
  Delta <- data.frame(matrix(NA, nrow=nrow(finalTable), ncol=length(delta)))
  colnames(Delta) <- delta
  ord <- c(1,2,11,3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)
  finalTable <- data.table(cbind(finalTable,Delta)) %>%
      setcolorder(ord)
  colsKeep <- !(grepl(pattern = "UNDISTURBED", colnames(finalTable))) # IF UNDISTURBED SHOULD BE COMPARED, THINK ON THIS LINE
  
  finalTable <- data.frame(finalTable)
  tabUndist <- finalTable[,!colsKeep]
  partTable <- finalTable[,colsKeep] %>%
        .[,-c(1, 8:10)]
 colsDelta <- grepl(pattern = "Delta", colnames(partTable))
 toAddValue <- which(colsDelta)
 
 for (i in 1:nrow(partTable)){
    j <-  which.min(partTable[i,])
    partTable[i,j+1] <- 0
    refVal <- partTable[i,j]
    for (t in 1:ncol(partTable)){
      ifelse(!is.na(partTable[i, t]),
        next,
        {partTable[i, t] <- partTable[i, t-1]-refVal})
      }
    }
    
 tableS1 <- cbind(Species = finalTable[,1], partTable,tabUndist)

write.csv(tableS1, file.path(outputPath(sim), "TableS1.csv"))
  
  return(tableS1)
}

