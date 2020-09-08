# Subsets Tables: Alberto's models

subsetTable <- function(dataset){

  ssTable <- data.table(questionGroup = names(dataset), sampleSize = NA)
  
  ssTable$sampleSize <- lapply(X = dataset, FUN = function(x){nrow(x)})
  order <- c(2, 4, 3, 2, 1, 6, 5)
  ssTable <- ssTable[order]
  
return(ssTable)

}