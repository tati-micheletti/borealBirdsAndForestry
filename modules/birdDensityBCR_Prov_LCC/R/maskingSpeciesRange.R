maskingSpeciesRange <- function(densityRasters = spRas,
                      sp = sp,
                      birdsRangeList = birdsRangeList){
  
      spRangePath <- birdsRangeList[[sp]]
      spRange <- preProcess(url = spRangePath, destinationPath = tempdir())
      densityRasMasked <- reproducible::maskInputs(x = densityRasters, rasterToMatch = spRange, 
                               maskWithRTM = TRUE, format = "GTiff")
  return(densityRasMasked)
}