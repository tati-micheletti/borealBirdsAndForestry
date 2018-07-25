individualFocal <- function(inList, inWeight, denomRas) {

  a <- raster::focal(inList, w = inWeight, na.rm = TRUE)
  b <- overlay(x = a, y = denomRas, fun = function(x,y) {return(x/y)})
  b[is.infinite(b)] <- 0 #some inf values returned from dividing by 0 (non-forest in LCC)

  return(b)
}