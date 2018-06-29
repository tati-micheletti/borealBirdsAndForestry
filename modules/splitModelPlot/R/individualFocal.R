individualFocal <- function(inList, inWeight, denomRas) {
  
  a <- raster::focal(inList, w = inWeight, na.rm = TRUE)
  b <- overlay(x = a, y = denomRas, fun = function(x,y) {return(x/y)})
  
  return(b)
}