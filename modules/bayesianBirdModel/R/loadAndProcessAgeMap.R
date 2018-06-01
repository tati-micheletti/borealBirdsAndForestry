loadAndProcessAgeMap <- function(dataPath = dataPath(sim), 
                                 projection = NULL,
                                 rP = sim$rP){
  
  if(!file.exists(file.path(dataPath,"can_age04_1km.tif"))){
    invisible(readline(prompt=paste0("ageMap was not found. Please download it manually at \n",
                                     "ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif",
                                     "\n and place it in: ",
                                     dataPath, ". \nOnce finished, press [enter] to continue.")))
  }
  ageMap <- raster::raster(file.path(dataPath,"can_age04_1km.tif"))
  if(!is.null(projection)){
#    ageMap <- projectInputs(ageMap, targetCRS = crs(projection)) # NOT WORKING (and the problem we have to solve which is not related to ): "no reprojecting because no rasterToMatch"
    extent(ageMap)[3:4] <- extent(ageMap)[3:4]*0.98 #"Witchcraft" to avoid error when reprojecting for areas too far north. Check https://gis.stackexchange.com/questions/220589/error-using-projectraster-in-r-error-in-if-maxy-miny-missing-value-whe
    ageMap <- Cache(raster::projectRaster, ageMap, crs = projection, method = 'bilinear')
    message(paste0("Reprojecting using "), projection)
  }
  ageMap[] <- round(ageMap[], 0)
  
  if(!is.null(rP)){
    ageMap <- crop(ageMap, rP) %>%
      mask(rP)
  }

  # cols <- length(which(!is.na(unique(getValues(ageMap)))))
  # ageMap <- setColors(ageMap, n=cols,colorRampPalette(c("LightGreen", "DarkGreen"))(cols)) #Make colors better but messes up map
  
  return(ageMap)
}
