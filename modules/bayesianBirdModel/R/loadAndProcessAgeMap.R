loadAndProcessAgeMap <- function(dataPath = dataPath(sim), projection = NULL){
  browser()
  if(!file.exists(file.path(dataPath,"can_age04_1km.tif"))){
    invisible(readline(prompt=paste0("ageMap was not found. Please download it manually at \n",
                                     "ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif",
                                     "\n and place it in: ",
                                     dataPath, ". \nOnce finished, press [enter] to continue.")))
  }
  ageMap <- raster::raster(file.path(dataPath,"can_age04_1km.tif"))
  if(!is.null(projection)){
#    ageMap <- projectInputs(ageMap, targetCRS = crs(projection)) # NOT WORKING: "no reprojecting because no rasterToMatch"
    ageMap <- raster::projectRaster(ageMap, crs = projection)
    message(paste0("Reprojecting using "), projection)
  }
  ageMap[] <- round(ageMap[], 0)
  cols <- length(which(!is.na(unique(getValues(ageMap)))))
  ageMap <- setColors(ageMap, n=cols,colorRampPalette(c("LightGreen", "DarkGreen"))(cols))
  
  return(ageMap)
}
