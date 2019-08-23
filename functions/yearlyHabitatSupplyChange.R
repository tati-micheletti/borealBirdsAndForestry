yearlyHabitatSupplyChange <- function(species, scale, folder){
  library(raster)
  library(data.table)
  fullTable <- rbindlist(lapply(X = species, FUN = function(sp){
    spTable <- rbindlist(lapply(X = scale, FUN = function(sc){
      ras <- raster::raster(grepMulti(x = list.files(folder, full.names = TRUE), 
                                      patterns = c("mergedTrend", sp, sc)))
      habitatChangePerYear <- sum(ras[], na.rm = TRUE)
      dt <- data.table::data.table(species = sp,
                                   scale = ifelse(sc == "100", "local", "neighborhood"),
                                   habitatChangePerYear = habitatChangePerYear)
      return(dt)
    })
    )
  return(spTable)
  })
  )
  return(fullTable)
}
