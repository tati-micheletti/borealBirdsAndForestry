jumboFocal <- function(inList = distStack, inWeight = focalMatrices, denomRas = LCFocals){
  
  b <- lapply(inList, FUN = individualFocal, inWeight = inWeight, denomRas = denomRas)
  
  return(b)
}

