# Extract results from models

extractResults <- function (sim.path, mySimOut, birdList){
  
  modelsNames <- names(mySimOut@.envir$models)
  extractedModels <- list()
 

for (namesModels in modelsNames)  {
  for (i in 1:length(birdList)){

    assign(namesModels, eval(parse(text = paste0(sim.path,namesModels,"[[",i,"]]@call"))))
    
    extractedModels[[namesModels]][[birdList[i]]] <- get(namesModels)
  }
}

  return(extractedModels)
}

extractModels <- extractResults("mySimOut$models$", mySimOut, birdList = objects$birdSpecies)

extracted <- unlist(extractModels)

sink("extractedModels.txt")
print(extracted)
sink()