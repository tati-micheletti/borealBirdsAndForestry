prepParamForChainCheck <- function(samplesList, param, unwantted){
  redChain <- lapply(samplesList, function(chain){
    nms <- usefun::grepMulti(pattern = param, unwanted = unwantted, x = colnames(chain))
    chanRed <- chain[, nms]
    return(chanRed)
    })
  return(redChain)
}
