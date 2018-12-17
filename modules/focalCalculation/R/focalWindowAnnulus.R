focalWindowAnnulus <- function(focalDistance = focalDistance, ras = Raster2){
  inMat <- focalWindow(x = Raster2[[1]], d = min(focalDistance))
  gc()
  outMat <- focalWindow(x = Raster2[[1]], d = max(focalDistance))
  #inverse the inner matrix
  inMat[inMat == 0] <- 1
  gc()
  inMat[inMat < 1] <- 0
  gc()
  #Get dimensions for matrix
  innerDim <- floor(dim(inMat)[1] / 2)
  gc()
  outerDim <- ceiling(dim(outMat)[1] / 2)
  gc()
  #Merge the two matrices
  outMat[(outerDim - innerDim):(outerDim + innerDim), 
         (outerDim - innerDim):(outerDim + innerDim)] <- inMat
  gc()
  #Recalculate the matrix value as 1/sum of non-zero values
  outMat[outMat > 0] <- 1 / length(outMat[outMat > 0])
  gc()
  focalMatrices <- outMat
  return(focalMatrices)
}