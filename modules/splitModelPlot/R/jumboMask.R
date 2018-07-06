jumboMask <- function(inputRas = binaryDisturb, 
                      inputMask = inputTiles$disturbanceYear, 
                      updateValue = 0, 
                      inverseLogic = TRUE,
                      mskVal = x){
  
  a <- mask(x = inputRas, mask = inputMask, maskvalue = mskVal, inverse = inverseLogic, updatevalue = updateValue)
  
  return(a)
}