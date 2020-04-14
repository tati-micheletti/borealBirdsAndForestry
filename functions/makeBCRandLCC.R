makeBCRandLCC <- function(pathData, overwrite = FALSE, RTM){
  source(file.path(getwd(), "functions/defineStudyArea.R"))
  rP <- Cache(defineStudyArea, testArea = TRUE,
              specificTestArea = "boreal",
              mapSubset = "Canada",
              destinationFolder = pathData)
  LCC05 <- LandR::prepInputsLCC(year = 2005, 
                                destinationPath = pathData, 
                                studyArea = rP,
                                overwrite = overwrite,
                                rasterToMatch = RTM)
  
  BCR <- Cache(prepInputs, url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip",
               targetFile = "BCR_Terrestrial_master.shp",
               alsoExtract = "similar",
               destinationPath = pathData,
               studyArea = rP,
               rasterToMatch = LCC05, 
               overwrite = overwrite, omitArgs = c("destinationPath", "overwrite"),
               userTags = c("objectName:BCR", "objectName2:updatedBCR"))
  return(list(BCR = BCR, LCC05 = LCC05))
}