# Method
if (FALSE){
  "LCC05 cropped to Managed/Unmanaged, summing all forest class landcover (1:15, 20, 32, 34:35) -- all forest"
  # --> Gives us total amount of managed forests, total amount of Unmanaged forests, total amount of Forests
  # --> Total Canadian forest
  # --> Total Canadian Managed Forested Area
  # --> Total Canadian Unmanaged Forested Area
  
  # --> Gives us total amount of boreal managed forests, total amount of boreal Unmanaged forests, total amount of boreal Forests 
  "LCC05 cropped to Brandt Shapefile AND Managed/Unmanaged, summing all forest class landcover (1:15, 20, 32, 34:35) -- only boreal forest"
  # --> Total Canadian Boreal forest
  # --> Total Canadian Boreal Managed Forested Area
  # --> Total Canadian Boreal Unmanaged Forested Area
  
  # --> I have to resample WW to 250m (as the 30m res can't fit to make the calculations, but resampling using 
  # billinear gives me the proportion of pixels that were disturbed. Then I just have to multiply the proportion by the 
  # area of the pixel -- now 250x250 instead of 30x30)
  "White & Wulder 2017 cropped to Managed/Unmanaged, summing all forest class landcover (1:15, 20, 32, 34:35) -- all forest"
  # --> Gives us total amount of managed forests, total amount of Unmanaged forests, total amount of Forests
  # --> Total Canadian forest harvested in area in ha
  # --> Total Canadian Managed Forested Area
  # --> Total Canadian Unmanaged Forested Area
  
  "White & Wulder 2017 cropped to both Brandt shapefile and Managed/Unmanaged, summing all forest class landcover (1:15, 20, 32, 34:35) -- only boreal forest"
  # --> Total Canadian Boreal forest
  # --> Total Canadian Boreal Managed Forested Area
  # --> Total Canadian Boreal Unmanaged Forested Area
  
  # NOTE: THIS IS NOT MERCHANTABLE FOREST! This is ALL types of forest (but only forest as classified by LCC05)
  
  # Classification
  "Total Canadian forest"
  "Total Canadian Managed Forested Area"
  "Total Canadian Unmanaged Forested Area"
  "Total Canadian Boreal forest"
  "Total Canadian Boreal Managed Forested Area"
  "Total Canadian Boreal Unmanaged Forested Area"
  
  
  library("SpaDES")
  library("raster")
  
  workDirectory <- getwd()
  message("Your current temporary directory is ", tempdir())
  tempFolder <- reproducible::checkPath(file.path(dirname(workDirectory), "tmp"), 
                                        create = TRUE)
  unixtools::set.tempdir(tempFolder)
  rasterOptions(default = TRUE)
  options(rasterTmpDir = tempFolder)
  setwd(file.path(getwd(), "bfbGIS"))
  SpaDES.core::setPaths(modulePath = file.path(getwd(), "modules"),
                        inputPath = file.path(getwd(), "inputs"),
                        outputPath = file.path(getwd(), "outputs"),
                        cachePath = file.path(getwd(), "cache"))
  
  # Step 1: load LCC05 layer with RTM of LCC05 with res 240x240m cropped to managed/unmanaged layer -- LCC05
  LCC05 <- LandR::prepInputsLCC(
    year = 2005,
    destinationPath = Paths$outputPath
  )
  LCC05_240m <- raster(extent(LCC05))
  res(LCC05_240m) <- c(240, 240)
  crs(LCC05_240m) <- crs(LCC05)
  
  BCR <- Cache(prepInputs, url = paste0("https://www.birdscanada.org/research/gislab",
                                        "/download/bcr_terrestrial_shape.zip"),
               targetFile = "BCR_Terrestrial_master.shp",
               alsoExtract = "similar",
               destinationPath = Paths$inputPath,
               rasterToMatch = LCC05_240m, 
               overwrite = overwrite, 
               omitArgs = c("destinationPath", "overwrite"),
               userTags = c("objectName:BCR_240m"))
  
  managedForest <- Cache(prepInputs, url = "https://drive.google.com/open?id=1tgqn8FajD1iSj0aECONGhFzwInau0-q8",
                         targetFile = "NIR2016_MF.shp", archive = "NIR2016_MF.zip",
                         alsoExtract = "similar",
                         # studyArea = studyArea, # prepInputs is stalling!
                         rasterToMatch = rasterToMatch,
                         overwrite = TRUE, omitArgs = c("overwrite"),
                         destinationPath = pathData,
                         userTags = c("objectName:managedForest",
                                      "function", "CacheFun:TRUE"))
  
  managedForest <- Cache(cropInputs, managedForest, studyArea = BCR) # Maybe a solution?
  
  LCC05_forest <- LandR::prepInputsLCC(
    year = 2005,
    destinationPath = Paths$outputPath,
    studyArea = managedForest,
    raterToMatch = LCC05_240m,
  )
  
  # Step 2: crop this managed/unmanaged LCC05 to boreal layer (without alpine and USA) -- LCC05boreal
  
  boreal <- defineStudyArea(testArea = TRUE, specificTestArea = "boreal", 
                            mapSubset = NULL)
  boreal <- projectInputs(boreal, targetCRS = crs(rasterToMatch))
  boreal <- Cache(cropInputs, boreal, studyArea = BCR)
  # Exclude USA and other countries and Alpine from the boreal
  toDropEco <- c("B_ALPINE", "H_ALPINE")
  toDropCountry <- c("USA", "GREENLAND", "FRANCE")
  boreal <- boreal[!boreal$TYPE %in% toDropEco & !boreal$COUNTRY %in% toDropCountry,]
  
  boreal <- prepInputs(url = "https://drive.google.com/open?id=1tgqn8FajD1iSj0aECONGhFzwInau0-q8",
                       targetFile = "NIR2016_MF.shp", archive = "NIR2016_MF.zip",
                       alsoExtract = "similar",
                       overwrite = TRUE, 
                       studyArea  = boreal,
                       omitArgs = c("overwrite"),
                       rasterToMatch = rasterToMatch, 
                       destinationPath = Paths$inputPath, 
                       userTags = c("objectName:studyArea",
                                    "obs: borealForestExtent",
                                    "obs2:OnlyCanadaNoAlpine"))
  
  LCC05_boreal <- LandR::prepInputsLCC(
    year = 2005,
    destinationPath = Paths$outputPath,
    studyArea = boreal,
    raterToMatch = LCC05_240m,
  )
  
  # Step 3: subset the layers into (i) managed forest; (ii) unmanaged forest; (iii) managed boreal forest; (iv) unmanaged boreal forest
  # shapfile layers managedForest and boreal
  unmanaged
  managed
  boreal_managed
  boreal_unmanaged
  
  # function for the next one: fasterize the shapefile, convert the landCoverClass values
  # Step 4: apply each of the 4 layers to the LCC05: - crop, mask and convert LCC05 (arg) non landCoverClass parameters (arg) into 0
  #                                                  - crop, mask and convert LCC05 (arg) landCoverClass into 6.25 (This is the corresponding area in ha per pixel)
  # I need a raster of 0/6.25 for each one of these. The summing happens on the table later
  # -- managed forest
  # -- unmanaged forest
  # -- managed boreal forest
  # -- unmanaged boreal forest
  
  # Step 5: load W&W tiled layers and aggregate using: tileXXAgg <- raster::aggregate(WWtileXX, fact = c(8, 8), expand = FALSE, method = 'bilinear')
  #                                --> This gives me the proportion of pixels that were harvested
  # Step 6: merge the tiles
  # Step 7: W&W and each of the LCC 4 layers to a data.table with pixelID
  # Step 8: Make new 2 columns (Total Forest; Total Boreal Forest) by summing the 2 correspondent columns
  # Step 9: Only exclude rows that have NA in all but pixelID columns 
  # Step 10: Convert WW column into proportion of area: WW * (250*250) = WWm2 --> to ha --> WWm2 * 0.0001 = WWha (or WW * 6.25 as shortcut)
  # Step 11: Multiply WWha by each of the columns, making 6 new columns
  # Step 12: Part I: sum of first 6 columns by column (DON'T multiply by 6.25 as its in ha already) and put in a new table:
  #                                               Total Managed, 
  #                                               Total Unmanaged, 
  #                                               Total Forest, 
  #                                               Total Boreal Managed, 
  #                                               Total Boreal Unmanaged, 
  #                                               Total Boreal Forest
  # Step 13: Part II: sum of last 6 columns by column (DON'T multiply by 6.25 as its in ha already) and put in a new table:
  #                                               Total Managed Harvested, 
  #                                               Total Unmanaged Harvested, 
  #                                               Total Forest Harvested, 
  #                                               Total Boreal Managed Harvested, 
  #                                               Total Boreal Unmanaged Harvested, 
  #                                               Total Boreal Forest Harvested
  # Step 14: make the percentage for each one of these 6 categories (columns -- i.e. one row with "Calculations in 27 years", and one with "Calculations per year",
  #                                             and an extra column with the data layers names (BRANDT, Junior's managed shp, WW 2017))
  # Step 15: Build the rest of the table rows with values from sources such as Gvt, W&W, and whatever else
  
  
}
