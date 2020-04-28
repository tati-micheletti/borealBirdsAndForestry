prepParamForChainCheck <- function(samplesList, param){
  redChain <- lapply(samplesList, function(chain){
    nms <- grep(pattern = param, x = colnames(chain))
    chanRed <- chain[, nms]
    return(chanRed)
    })
  return(redChain)
}
