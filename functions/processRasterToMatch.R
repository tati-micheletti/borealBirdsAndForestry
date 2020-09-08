processRasterToMatch <- function(RTMpath,
                                 rasterToMatch,
                                 studyArea,
                                 reproducibleSHA){
  
  if (!file.exists(RTMpath)){
    t0 <- Sys.time()
    browser()
    rasterToMatch <- postProcess(rasterToMatch,
                                 studyArea = studyArea,
                                 filename2 = "RTMmasked",
                                 destinationPath = SpaDES.core::Paths$inputPath,
                                 useGDAL = 'force',
                                 userTags = c("objectName:rasterToMatch", stepCacheTag,
                                              "outFun:Cache", paste0("reproducibleCommit:", 
                                                                     reproducibleSHA), 
                                              "goal:RTM"),
                                 omitArgs = c("overwrite", "destinationPath", "filename2"))
    # As the masked part is not NA, but 0's, we need to NA those.
    rasterToMatch[rasterToMatch == 0] <- NA
    writeRaster(x = rasterToMatch, 
                filename = RTMpath, 
                format = "GTiff")
    rasterToMatch <- raster(RTMpath)
  } else {
    rasterToMatch <- raster(RTMpath)
  }
  return(rasterToMatch)
}