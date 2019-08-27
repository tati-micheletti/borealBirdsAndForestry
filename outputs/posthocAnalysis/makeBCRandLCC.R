makeBCRandLCC <- function(pathData){
  source(file.path(getwd(), "functions/defineStudyArea.R"))
  rP <- Cache(defineStudyArea, testArea = TRUE,
              specificTestArea = "boreal",
              mapSubset = "Canada",
              destinationFolder = pathData)#, cacheId = "3a17633692bc3299") # cacheId = 3a17633692bc3299 as it is not picking up
  
  LCC05 <- Cache(prepInputs, url = "https://drive.google.com/open?id=19rMA800ZFsKkXx-eqBcXdR84P9MQdCLX",
                 destinationPath = pathData,
                 studyArea = rP,
                 overwrite = FALSE,
                 userTags = "objectName:LCC05", omitArgs = c("overwrite", "destinationPath"))
  targetCRS <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  LCC05 <- Cache(projectInputs, LCC05, targetCRS = targetCRS, userTags = "objectName:LCC05reproj", omitArgs = c("overwrite", "destinationPath"))#, cacheId = "e10b12018c04b906") # cacheId = e10b12018c04b906  as it is not picking up
  BCR <- Cache(prepInputs, url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip",
               targetFile = "BCR_Terrestrial_master.shp",
               alsoExtract = "similar",
               destinationPath = pathData,
               studyArea = rP,
               rasterToMatch = LCC05, overwrite = FALSE, omitArgs = c("destinationPath", "overwrite"),
               userTags = c("objectName:BCR", "objectName2:newBCR")) # cacheId = e10b12018c04b906 as it is not picking up
  return(list(BCR = BCR, LCC05 = LCC05))
}