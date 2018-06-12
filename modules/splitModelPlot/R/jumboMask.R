jumboMask <- function(inputRas, inputMask, maskValue, inverseLogic, updateValue){
 
  a <- mask(x = inputRas, mask = inputMask, maskvalue = maskValue, inverse = inverseLogic, updatevalue = updateValue)
  return(a)
}