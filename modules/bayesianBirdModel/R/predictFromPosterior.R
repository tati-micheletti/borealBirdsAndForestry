predictFromPosterior <- function(currentTime,
                                  pathData,
                                  dataForPrediction,
                                 species,
                                  covPars){
  
  message(paste0("Predicting for",
                 " year ", crayon::yellow(currentTime), 
                 " for ", crayon::yellow(species), "(Time:", Sys.time(), ")"))
  
  # For each row in the table, I need to randomly select a row from the full posterior, and multiply
  colsToKeep <- names(dataForPrediction)[!names(dataForPrediction) == "pixelID"]
  mat <- dataForPrediction[, ..colsToKeep] # 1 is pixelID

  pars <- cbind(data.table(logDensity = 1), covPars[sample(nrow(covPars), 
                                                           size = NROW(mat), 
                                                           replace = TRUE), ])
  # 1 = multiplier of density, others are beta
  set(x = dataForPrediction, i = NULL, 
      j = paste0("Year", currentTime), 
      value = exp(rowSums(mat*pars)))
  
  # Make a new column for original density
  dataForPrediction[, "originalDensity" := exp(get(paste0("log", species)))]
  
  colsToRemove <- names(dataForPrediction)[!names(dataForPrediction) %in% c("pixelID", "originalDensity",
                                                                        paste0("Year", currentTime))]
  dataForPrediction[, (colsToRemove) := NULL]
  
  return(dataForPrediction)
}
