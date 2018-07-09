jumboMask <- function(inputRas = binaryDisturb, 
                      inputMask = inputTiles$disturbanceYear, 
                      updateValue = 0, 
                      inverseLogic = TRUE,
                      mskVal = x,
                      spName = spName){
  
  message(crayon::yellow(paste0("Masking 19", mskVal, " tiles for ", spName)))
  a <- mask(x = inputRas, mask = inputMask, maskvalue = mskVal, inverse = inverseLogic, updatevalue = updateValue)
  
  return(a)
}