percentOfAreaAffectedByDisturbance <- function(tableWithChange, logicalColChange){
  # percent area that is affected by ANY change (even if 1 pixel, within the 500m annulus buffer) 
  # change of total area of non-NA pixels for a given bird
  tb <- table(tableWithChange[[logicalColChange]])
  percAffected <- tb["TRUE"]/tb["FALSE"]
  names(percAffected) <- "percentAreaAffectedByDisturbance"
  return(percAffected)
}