centerColMeans <- function(x) {
  
  xcenter <- colMeans(x)
  newCols <- x - rep(xcenter, rep.int(nrow(x), ncol(x)))
  
  return(newCols)
}