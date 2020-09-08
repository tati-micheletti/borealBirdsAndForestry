# checking for all duplicated values

checkDuplicates <- function(dataTable, ignoreCols){

require(data.table)
  
  dataTable.Duplicated <- subset(x = dataTable, select = !(names(dataTable) %in% ignoreCols))
  dataTable.Duplicated2 <- dataTable.Duplicated
  dataTable.Duplicated2$Dups1 <- duplicated(dataTable.Duplicated)
  dataTable.Duplicated2$Dups2 <- duplicated(dataTable.Duplicated, fromLast = TRUE)
  
  dataTable <- dataTable.Duplicated2[Dups1 == TRUE | Dups2 == TRUE,]
  whichRows <- which(dataTable.Duplicated2[,Dups1 == TRUE | Dups2 == TRUE,])
  
  duplicates <- list(DATA=dataTable, ROWS = whichRows)
 
  return(duplicates)
}