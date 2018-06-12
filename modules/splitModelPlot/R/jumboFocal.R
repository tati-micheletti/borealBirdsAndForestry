jumboFocal <- function(inList, inWeight, denomRas){
  
  a <- lapply(inList, FUN = raster::focal, w = inWeight, na.rm = TRUE)
  
  b <- lapply(a, FUN = function(x, y = denomRas){
    
    out <- overlay(x, y, fun = function(x,y) {return(x/y)})
    
    return(out)
  })
  return(b)
}