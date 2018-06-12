groupSplitRaster <- function(spec, mod, abund, sim) {
  rasterList <- list("distType" = sim$disturbanceType, "distYear" = sim$disturbanceYear, "land" = sim$landCover) 
  rasterList[["inputAbundances"]] <- abund
  newlist <- Cache(Map, rasterList, path = file.path(dataPath(sim), names(rasterList)), f =  splitRaster, 
                 MoreArgs = list(nx = P(sim)$nx, ny = P(sim)$ny, buffer = P(sim)$buffer, rType = P(sim)$rType))
 
  lengthvect <- 1:(P(sim)$nx*P(sim)$ny)
  outList <- lapply(lengthvect, FUN = tileReorder, inList = newlist, origList = rasterList, sim = sim, passedModel = mod)
  
  #Merge will not work if the list is named. Investigate why one day. 
  rasList <- lapply(outList, function(s) s[[1]])
  mergePlot <- mergeRaster(rasList) #recombine tiles into single raster layer
  
  #Make time series from sum
  timeSums <- lapply(outList, function(s) s[[2]]) %>%
    list.stack(.) %>%
    apply(., MARGIN = 2, FUN = sum)
  sumTS <- ts(timeSums, start = start(sim) + 1900, end = end(sim) + 1900, frequency = 1)
  timePlot <- ggplot2::autoplot(sumTS)+
    ggplot2::theme_bw() +
    ggplot2::labs(title = paste(spec, "population"),
                  y = "Predicted population",
                  x = "year") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                   panel.grid.minor = ggplot2::element_blank())
    
    
    
           
  #Return time series and raster as list
  outList <- list("trendRas" = mergePlot, "timePlot" = timePlot, "population" = sumTS)
  
  return(outList)
}
