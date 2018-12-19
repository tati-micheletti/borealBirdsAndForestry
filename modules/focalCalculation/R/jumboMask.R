jumboMask <- function(inputRas = binaryRaster1,
                      inputMask = Raster3, updateValue = 0,
                      inverseLogic = TRUE, 
                      maskValue = maskValue, 
                      currentYear = currentYear){
browser()
  yearValue <- getValues(inputMask)
  yearValue[yearValue %in% mskVal] <- 999
  inputMask <- setValues(inputMask, yearValue)
  
  message(crayon::yellow(paste0("Masking year ", x, " tiles for ", spName)))
  a <- mask(x = inputRas, mask = inputMask, maskvalue = 500, inverse = inverseLogic, updatevalue = updateValue)
  
  return(a)
}