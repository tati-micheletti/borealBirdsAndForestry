loadAndProcessAgeMap <- function(dataPath, 
                                 projection = NULL,
                                 filename2){
  
  ageMap <- Cache(prepInputs, url = "https://drive.google.com/open?id=1kiXmbBU3zy-92fS4Bt5gmLcsF15L25sG",
                       filename2 = filename2, targetFile = "can_age04_1km.tif",
                  destinationPath = dataPath, format = "GTiff",
                       overwrite = TRUE)
  
  if(!is.null(projection)){
#    ageMap <- projectInputs(ageMap, targetCRS = crs(projection)) # NOT WORKING (and the problem we have to solve which is not related to ): "no reprojecting because no rasterToMatch"
    extent(ageMap)[3:4] <- extent(ageMap)[3:4]*0.98 #"Witchcraft" to avoid error when reprojecting for areas too far north. Check https://gis.stackexchange.com/questions/220589/error-using-projectraster-in-r-error-in-if-maxy-miny-missing-value-whe
    ageMap <- Cache(projectInputs, ageMap, targetCRS = crs(projection), 
                    filename2 = filename2, userTags = c("AgeMapreproject"),
                    format = "GTiff", overwrite = TRUE)
    message(paste0("Reprojecting using "), projection)
  }
  ageMap[] <- round(ageMap[], 0)

  return(ageMap)
}
