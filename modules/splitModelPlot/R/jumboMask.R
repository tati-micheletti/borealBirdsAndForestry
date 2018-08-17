jumboMask <- function(inputRas = binaryDisturb, #Disturbances that area due to the Disturbance Class (log or fire or whatever)
                      inputMask = inputTiles$disturbanceYear, # All disturbances in every year
                      updateValue = 0,
                      inverseLogic = TRUE,
                      mskVal = maskValue,
                      spName = spName,
                      x = x){

  yearValue <- getValues(inputMask)
  yearValue[yearValue %in% mskVal] <- 500
  inputMask <- setValues(inputMask, yearValue)
  
  message(crayon::yellow(paste0("Masking year ", x, " tiles for ", spName)))
  a <- mask(x = inputRas, mask = inputMask, maskvalue = 500, inverse = inverseLogic, updatevalue = updateValue)
  
  return(a)
}