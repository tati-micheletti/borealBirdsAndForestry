MergeDistances <- function(inList, times, abund, passedModel){
  
  mergeList <- rlist::list.cbind(inList) %>%
    list.parse(.)
  #Make raster stack with focal distance rasters for every year. 
  stackDistances <- lapply(mergeList, FUN = stack)
  #for each raster in stackDistances (ie focal/year combo) fit model 
  out <- lapply(stackDistances, function(q, abundRas = abund){
    
    #Make a stack of the disturbed area in a single year and the expected abundance. Use to predict with fitModel 
    names(abund) <- 'abundance'
    tempStack <- stack(q, abundRas) 
    p <- fitModel(inRas = tempStack, inputModel = passedModel)
    return(p)
  })
  
  #Find sum of each raster for TS
  sumTS <- lapply(out, FUN = function(x) {
    v <- getValues(x)
    s <- sum(v, na.rm = TRUE)
    return(s)
  })
  
  #Stack rasters in the list
  predictedStack <- raster::stack(out)
  
  #Use model predictions to predict slope over time
  modPredict <- raster::calc(predictedStack, fun = function(predictedStack, years = times){
    
    rts <- ts(predictedStack, start = years[1], end = tail(years, n=1), frequency = 1)
    if(length(rts[is.na(rts) == TRUE])> 0){
      return(NA) #have to double check what tod o here
    }else{
      out <- trend::mk.test(rts)
    #Returns kendalls tau. Can obviously implement something better. Sum time series, return as separate object. ####FIX THIS#### 
      s <- out$estimates[3]
      return(s)
    }
  })
  mod <- list(modPredict, sumTS)
  return(mod)
}
