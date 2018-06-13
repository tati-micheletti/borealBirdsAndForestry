jumboMask <- function(inputRas = binaryDisturb, 
                      inputMask = inputTiles$distYear, 
                      updateValue = 0, 
                      inverseLogic = TRUE){
 
  a <- mask(x = inputRas, mask = inputMask, maskvalue = maskValue, inverse = inverseLogic, updatevalue = updateValue)
  return(a)
}