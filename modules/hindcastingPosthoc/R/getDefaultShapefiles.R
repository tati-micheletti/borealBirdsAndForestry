getDefaultShapefiles <- function(rasterToMatch, 
                                 studyArea, 
                                 pathData){
  
  # 1. Load a shapefile and extract the ID by pixel (i.e. have a dt with pixelID and a new column region)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# MANAGED FOREST VS NON-MANAGED FOREST #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
managedForest <- Cache(prepInputs, url = "https://drive.google.com/open?id=1tgqn8FajD1iSj0aECONGhFzwInau0-q8",
                       targetFile = "NIR2016_MF.shp", archive = "NIR2016_MF.zip",
                       alsoExtract = "similar",
                       studyArea = studyArea,
                       rasterToMatch = rasterToMatch,
                       overwrite = TRUE, omitArgs = c("overwrite"),
                       destinationPath = pathData,
                       userTags = c("objectName:managedForest",
                                    "function", "CacheFun:TRUE"))

managedForestDT <- returnDTfromShapefile(shp = managedForest,
                                         rasterToMatch = rasterToMatch,
                                         fieldToUse = "ManagedFor",
                                         namesVector = c("Managed", "Unmanaged"),
                                         columnName = "forest")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# MANAGED FOREST FOR EACH PROVINCE AND BCR #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

BCR <- Cache(prepInputs, url = paste0("https://drive.google.com/open?id=1VlYGtmFrgaYHY1dfl0kWOQH62PaH2YnP"),
             targetFile = "BCR_Terrestrial_master.shp",
             archive = "bcr_terrestrial_shape.zip",
             alsoExtract = "similar",
             destinationPath = pathData,
             overwrite = TRUE,
             studyArea = studyArea,
             rasterToMatch = rasterToMatch,
             omitArgs = c("useCache", "overwrite"),
             userTags = c("objectName:BCRLCC05",
                          "function:getDefaultShapefiles",
                          "CacheFun:TRUE"))
# NOTE: There are a few pixels from the USA and water pixels that would remain
# in the shapefile, so we clear that first

colsToDrop <- c("BCRNAME", "COUNTRY", "REGION", "WATER", "Shape_Leng", "Shape_Area", "Id")
# The BCR shapefile has some sort of water, non-water polygons. Need to drop those water ones before doing this!
# waterSites <- 

# WATER == 1 needs to be removed, and so does country == USA
BCR <- BCR[BCR$WATER == 3 & BCR$COUNTRY == "CANADA" ,
           !(names(BCR) %in% colsToDrop)]

BCRDT <- returnDTfromShapefile(shp = BCR,
                               rasterToMatch = rasterToMatch,
                               fieldToUse = "BCR",
                               columnName = "BCR")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~DO THE SAME FOR PROVINCE

provinceDT <- returnDTfromShapefile(shp = BCR,
                               rasterToMatch = rasterToMatch,
                               fieldToUse = "PROVINCE_S",
                               columnName = "province")
# There are NA's in the provinces as some provinces are outside the boreal forest

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# STUDY AREA ALBERTA #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

albertaSA <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Ix_FgdB6UNvu_K3-ZJ_ibLpW3Fg2G0k8",
                   targetFile = "ecodistricts_ALPAC.shp",
                   archive = "ecodistricts_ALPAC.zip",
                   alsoExtract = "similar",
                   studyArea = studyArea,
                   rasterToMatch = rasterToMatch,
                   overwrite = TRUE, omitArgs = c("overwrite"),
                   destinationPath = pathData,
                   userTags = c("function:getDefaultShapefiles",
                                "objectName:albertaSA",
                                "CacheFun:TRUE"))

ALPACDT <- returnDTfromShapefile(shp = albertaSA,
                                    rasterToMatch = rasterToMatch,
                                    columnName = "ALPAC")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# STUDY AREA QUEBEC #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ForetMontmorrency <- Cache(prepInputs, url = "https://drive.google.com/open?id=1zTvVqFi4-rFjVjQL0g_Dk-op56aIQDUq",
                  targetFile = "FM2014_polygons.shp",
                  archive = "FM2014_polygons.zip",
                  alsoExtract = "similar",
                  studyArea = studyArea,
                  rasterToMatch = rasterToMatch,
                  overwrite = TRUE,
                  omitArgs = c("overwrite"),
                  destinationPath = pathData,
                  userTags = c("function:getDefaultShapefiles",
                               "objectName:ForetMontmorrency", 
                               "CacheFun:TRUE"))

ForetMontmorrencyDT <- returnDTfromShapefile(shp = ForetMontmorrency,
                                 rasterToMatch = rasterToMatch,
                                 columnName = "ForetMontmorrency")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# STUDY AREA CALLING LAKE #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  CallingLake <- Cache(prepInputs, url = "https://drive.google.com/open?id=1CqE9aDBYN9GZ6iEBL9xEhj8hNTb9aDoz",
                            targetFile = "NewCLShapefile.shp",
                            archive = "NewCLShapefile.zip",
                            rasterToMatch = rasterToMatch,
                            studyArea = studyArea,
                            destinationPath = pathData, 
                            overwrite = TRUE,
                            fun = "raster::shapefile",
                            userTags = c("function:getDefaultShapefiles",
                                         "objectName:CallingLake",
                                         "CacheFun:TRUE"))

  # Calling lake has no polygon, so I have to draw it
  dat <- coordinates(CallingLake)
  ch <- chull(dat)
  coords <- dat[c(ch, ch[1]), ]  # closed polygon
  sp_poly <- SpatialPolygons(list(Polygons(list(Polygon(coords)), ID = 1)), proj4string = crs(CallingLake))
  sp_poly_df <- SpatialPolygonsDataFrame(sp_poly, data = data.frame(ID = 1))
  sp_poly_dfProj <- projectInputs(sp_poly_df, targetCRS = crs(rasterToMatch))
  CallingLake <- gBuffer(spgeom = sp_poly_dfProj, byid = FALSE, width = 10^4)
  CallingLake$value <- 1
  
  CallingLakeDT <- returnDTfromShapefile(shp = CallingLake, 
                                         rasterToMatch = rasterToMatch,
                                         columnName = "CallingLake")

  return(list(managedForest = managedForestDT, 
              BCR = BCRDT,
              province = provinceDT,
              ALPAC = ALPACDT,
              ForetMontmorrency = ForetMontmorrencyDT,
              CallingLake = CallingLakeDT))
}