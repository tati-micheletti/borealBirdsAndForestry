options(reproducible.useCache = TRUE)
library("reproducible")
library("raster")
library("data.table")
library("usefun")
library("magrittr")
wd <- getwd()
if (all(pemisc::user() %in% c("Tati", "tmichele"), wd != "/mnt/data/Micheletti/borealBirdsAndForestry")){
  warning("Make sure you are in the correct working directory!")
  setwd("/mnt/data/Micheletti/borealBirdsAndForestry")
}
googledrive::drive_deauth()
SpaDES.core::setPaths(cachePath = file.path(getwd(), "cache"))
# 1. Calculate prediction area for each bird species: `birdSpecies` and `predArea`
# 2. Estimate total abundance from Peter&Diana's paper for this area * 6.25: `realAbund0`
# 1.1. Before I can calculate total abundance, I need to mask out all the non-NA pixels 
# from the expected density rasters because I have NA's in the focal rasters (i.e. lakes, 
# rivers, mountains, etc?), so we need to have exactly the same pixels as NA's in the 
# expected density and predicted rasters

# FIRST, TEST IF THE FILE ALREADY EXISTS | If not, create it
completeSummaryFile <- file.path(wd, "outputs/posthocAnalysis", "completeSummaryTable.rds")
if (file.exists(completeSummaryFile)){
  fullTableList <- readRDS(completeSummaryFile) # JUST FOR NOW!
} else {
originalDensFolder <- file.path(wd, "modules/birdDensityBCR_Prov_LCC/data")
maskedDensityRasFolder <- file.path(wd, "outputs/posthocAnalysis/maskedDensityRas")
  fl <- usefun::grepMulti(x = list.files(path = originalDensFolder,
                                         full.names = TRUE), patterns = c("density", ".tif"))
  fl <- unlist(lapply(X = fl, FUN = function(ras){
    originalRas <- raster::raster(ras)
    bird <- usefun::substrBoth(strng = names(originalRas), howManyCharacters = 4, fromEnd = TRUE)      
    newRasName <- usefun::grepMulti(x = list.files(path = maskedDensityRasFolder,
                                           full.names = TRUE), patterns = c("density", bird,".tif"))
    if (length(newRasName)!=0){
      message(crayon::green(bird, " new density rasters already exist. Returning."))
      return(newRasName)
    } else {
      message(crayon::white(bird, " new density rasters don't exist. Creating..."))
      RTM <- raster::raster(file.path(wd, "modules/predictBirds/data/", paste0("predicted", bird, "500mYear1984.tif")))
      newRasName <- file.path(maskedDensityRasFolder, names(originalRas))
      maskedRas <- postProcess(originalRas, rasterToMatch = RTM, 
                               maskWithRTM = TRUE, format = "GTiff",
                               filename2 = newRasName)
      return(newRasName) 
    }
  }))

# DENSITY
densityFullTable <- file.path(wd, "outputs/posthocAnalysis", 
                               "fullTableDENSITY.rds")
summarizedTableFileName <- file.path(wd, "outputs/posthocAnalysis", 
                                     "birdsTableDENSITY.rds")
fl <- usefun::grepMulti(x = list.files(path = maskedDensityRasFolder, 
                                       full.names = TRUE), patterns = c("density", ".tif"))
source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/returnBirdAbundance.R')
if (!file.exists(densityFullTable)){
  densityTable <- returnBirdAbundance(filepath = fl, type = "density", 
                                      fullTableFilename = densityFullTable, 
                                      summarizedTableFileName = summarizedTableFileName) 
} else {
  densityTable <- readRDS(summarizedTableFileName)
}


# 3. Make one more prediction for each species: set disturbance to zero completely. `abund1984` # ASSUMING pre1985 there is no disturbance
# 3.1. Create a focal raster called mergedFocal1984-500Res250m.tif
fullTableListFile <- file.path(wd, "outputs/posthocAnalysis/birdsTableCOMPLETE.rds")
if (!file.exists(fullTableListFile)){
  folderFocalRas <- file.path(wd, "modules/focalCalculation/data")
name1984Ras <- "mergedFocal1984-500Res250m.tif"
if (!file.exists(file.path(folderFocalRas, name1984Ras))){
  focal1984 <- raster::raster(file.path(folderFocalRas, "mergedFocal1985-500Res250m.tif"))
  vals1984 <- data.table::data.table(getValues(focal1984))
  vals1984[!is.na(vals1984)] <- 0
  focal1984 <- setValues(x = focal1984, values = vals1984$V1)
  writeRaster(focal1984, file.path(folderFocalRas, name1984Ras), format = "GTiff")
}
# 3.2. Run bird predictions for all species over this raster
pred1984path <- file.path(getwd(), "outputs/08JUL2019_nonSignificant/prediction1984_500.rds")
if (!file.exists(pred1984path)){
  source(file.path(getwd(), "predict1984.R"))
} else {
  prediction1984_500 <- readRDS(pred1984path)
}}

# 4. Calculate abundYYYY and the rate of change from 1984 to 1985: `rate1985`(using `(1985-Abund0)/Abund0`) (THIS PER PIXEL AND TOTAL)
# 4.1 ABUND YEARS 1984 - 2011
if (file.exists(fullTableListFile)){
  fullTableList <- readRDS(fullTableListFile) 
} else {
  fullTableListFile <- makeBirdTable(tableFileName = "birdsTableCOMPLETE", 
                                     folderForTables = file.path(wd, "outputs/posthocAnalysis"), 
                                     folderForPredictedRasters = file.path(wd, "modules/predictBirds/data/"), 
                                     locationReturnBirdAbundanceFUN = file.path(wd, "functions/returnBirdAbundance.R"),
                                     typeOfTable = "summarizedTable")
  fullTableList <- readRDS(fullTableListFile)
}
fullTableList <- usefun::cbindFromList(lst = fullTableList)

# 4.2 RATE YEARS 1984 - 2011
library("data.table")

# 4.2.1 Rate per year
abundNames <- usefun::grepMulti(names(fullTableList), patterns = c("abund"))
for (i in 2:length(abundNames)) {
  yearRef <- usefun::substrBoth(strng = abundNames[i],
                                howManyCharacters = 4,
                                fromEnd = TRUE)
  thisYearsRate <- (fullTableList[,abundNames[i], with = FALSE]-
                      fullTableList[,abundNames[i-1], with = FALSE])/
    fullTableList[,abundNames[i-1], with = FALSE]
  names(thisYearsRate) <- paste0("rate", yearRef)
  fullTableList <- cbind(fullTableList, thisYearsRate)
}
# 4.2.2. Rate compared to abund1984: cmmRate
for (i in 2:length(abundNames)) {
  yearRef <- usefun::substrBoth(strng = abundNames[i],
                                howManyCharacters = 4,
                                fromEnd = TRUE)
  thisYearsRate <- (fullTableList[,abundNames[i], with = FALSE]-
                      fullTableList[,"abund1984"])/
    fullTableList[,"abund1984"]
  names(thisYearsRate) <- paste0("cummRate", yearRef)
  fullTableList <- cbind(fullTableList, thisYearsRate)
}

# 5. Calculate the AbundXXXX in each year: `realAbundXXXX` (using `realAbund0`+(`realAbund0`*`cummRateXXXX`))

# Now real density
spDensityTable <- lapply(X = species, FUN = function(sp){
  fl <- usefun::grepMulti(x = list.files(path = maskedDensityRasFolder,
                                         full.names = TRUE), patterns = c("density", sp,".tif"))
  fullDensityTable <- returnBirdAbundance(filepath = fl, type = "density",
                                          fullTableFilename = paste0("densityFullTable", sp),
                                          summarizedTableFileName = paste0("summarizedTableFileName", sp),
                                          whichToLoad = "fullTable")
})
names(spDensityTable) <- species

# 5.1 Attach the `realAbund0` to the full table
fullTableList <- usefun::cbindFromList(list(fullTableList, densityTable)) # BEFORE MERGING CHECK IF BOTH TABLES HAVE THE SAME VALUES FOR TOTAL PIXELS!
# 5.2 calculating the real abundance (based on the original densities and the rate of loss)
cummRates <- usefun::grepMulti(x = names(fullTableList), patterns = "cumm")
lapply(X = cummRates, FUN = function(cum){
  newColName <- paste0("realAbund", 
                       usefun::substrBoth(strng = cum, 
                                          howManyCharacters = 4, 
                                          fromEnd = TRUE))
  fullTableList[, c(newColName) := realAbund0+(get(cum)*realAbund0)]
})
saveRDS(fullTableList, file = completeSummaryFile)
}

# CALCULATE ALL THE SAME I DID FOR THE SUMMARY, but at pixel basis!
# NOW DO THE SAME I DID FOR THE SUMMARIZED TABLE, BUT FOR EACH PIXEL
# FOR THAT, NEED TO DO FOR EACH BIRD

# Pixel by pixel basis: 
source(file.path(wd, "functions/makeBirdTable.R"))
tableFileName <- "birdsTableAbundFull"
folderForTables <- file.path(wd, "outputs/posthocAnalysis")
fullTablePixelsFile <- file.path(folderForTables, paste0(tableFileName, ".rds"))
if (file.exists(fullTablePixelsFile)){
  fullTablePixels <- readRDS(fullTablePixelsFile)
} else {
  species <- c("BBWA", "BLPW", "BOCH", "BRCR", "BTNW", "CAWA", "CMWA", "CONW", "OVEN", "PISI", "RBNU", "SWTH", "TEWA", "WETA", "YRWA")
  spFullTable <- lapply(species, FUN = function(sp){
    tableFileName <- paste0("birdsTableAbund",sp)
    fullTablePixels <- makeBirdTable(species = sp, tableFileName = tableFileName,
                                     folderForTables = folderForTables,
                                     folderForPredictedRasters = file.path(wd, "modules/predictBirds/data/"),
                                     locationReturnBirdAbundanceFUN = file.path(wd, "functions/returnBirdAbundance.R"),
                                     typeOfTable = "fullTable", lightLoad = FALSE)
  })
}
names(spFullTable) <- species

fullTableAllBirds <- lapply(X = species, function(bird){
  completeSummaryFile <- file.path(wd, "outputs/posthocAnalysis", paste0("fullPixelTable", bird, ".rds"))
  if (file.exists(completeSummaryFile)){
    message(crayon::green("Full pixel table exists for ", bird))
    return(completeSummaryFile)
  } else {
    message(crayon::yellow("Full pixel table doesn't exist for ", bird, ". Creating..."))
    
    # 4.2 RATE YEARS 1984 - 2011 PER BIRD
    library("data.table")
    
    # 4.2.1 Rate per year
    fullTableList <- readRDS(spFullTable[[bird]])
    dcastedTable <- dcast(data = fullTableList, formula = species + pixelID ~ year, value.var = "density")
    ys <- usefun::substrBoth(unique(fullTableList$year), howManyCharacters = 4, fromEnd = TRUE)
    newNames <- c(names(dcastedTable)[1:2], paste0("abund", ys))
    names(dcastedTable) <- newNames
    fullTableList <- dcastedTable
    abundNames <- usefun::grepMulti(names(fullTableList), patterns = c("abund"))
    for (i in 2:length(abundNames)) {
      yearRef <- usefun::substrBoth(strng = abundNames[i],
                                    howManyCharacters = 4,
                                    fromEnd = TRUE)
      thisYearsRate <- (fullTableList[,abundNames[i], with = FALSE]-
                          fullTableList[,abundNames[i-1], with = FALSE])/
        fullTableList[,abundNames[i-1], with = FALSE]
      names(thisYearsRate) <- paste0("rate", yearRef)
      fullTableList <- cbind(fullTableList, thisYearsRate)
    }
    # 4.2.2. Rate compared to abund1984
    for (i in 2:length(abundNames)) {
      yearRef <- usefun::substrBoth(strng = abundNames[i],
                                    howManyCharacters = 4,
                                    fromEnd = TRUE)
      thisYearsRate <- (fullTableList[,abundNames[i], with = FALSE]-
                          fullTableList[,"abund1984"])/
        fullTableList[,"abund1984"]
      names(thisYearsRate) <- paste0("cummRate", yearRef)
      fullTableList <- cbind(fullTableList, thisYearsRate)
    }
    # 5. Calculate the AbundXXXX in each year: `realAbundXXXX` (using `realAbund0`+(`realAbund0`*`cummRateXXXX`))
    # 5.1 Attach the `realAbund0` to the full table
    fullTableList <- usefun::cbindFromList(list(fullTableList, spDensityTable[[bird]])) # BEFORE MERGING CHECK IF BOTH TABLES HAVE THE SAME VALUES FOR TOTAL PIXELS!
    names(fullTableList)[names(fullTableList) == "density"] <- "realAbund0"
    # 5.2 calculating the real abundance (based on the original densities and the rate of loss)
    cummRates <- usefun::grepMulti(x = names(fullTableList), patterns = "cumm")
    lapply(X = cummRates, FUN = function(cum){
      newColName <- paste0("realAbund", 
                           usefun::substrBoth(strng = cum, 
                                              howManyCharacters = 4, 
                                              fromEnd = TRUE))
      fullTableList[, c(newColName) := realAbund0+(get(cum)*realAbund0)]
    })
    saveRDS(fullTableList, file = completeSummaryFile)
    return(completeSummaryFile)
  }
})

# To correct for LCC previous to 2005 that we don't know what was, we can use the minimum and the maximum values of 
# each one of the forest cover types that were harvested until 2011 for each pixel.
# 1. Layover LCC05 on 2011 disturbance map. Get all unique LCC classes
# 2. Lookout table BCR Prov LCC ExpD per species with only forested types that were harvested in 2011 (as 2011 is cummulative), from 1.
# 3. Based on pixelID, create new columns in each table (fullTableList) realAbundMinXXXX, and  realAbundMaxXXXX, realAbundAveXXXX (where XXXX is 1984-2004)
# 4. Subtract realAbundMin1984, realAbundMax1984 and realAbundMax1984 rasters from realAbund2011 to get the lossAbundanceMin, lossAbundanceMax, lossAbundanceAverage
# *** Shouldn't I also consider the CV for these values? i.e. min = minValuesOfForestTypes*(averageExpD-cvExpD); max = maxValueOfForestTypes*(averageExpD+cvExpD)

# Testing if Density rasters are the same format as our tables from the birdDensityBCR_Prov_LCC
birdSp <- "BBWA"
BBWA <- readRDS(fullTableAllBirds[[1]]) # hardcoded... [ FIX ] # THIS IS MY DENSITY RASTER (ExpD)
templateRas <- raster(file.path(getwd(), paste0("modules/birdDensityBCR_Prov_LCC/data/density",birdSp,".tif")))
dtTemplateRas <- data.table::data.table(pixelID = 1:ncell(templateRas), vals = raster::getValues(templateRas)) # making sure these are the rasters we need. 
# If the NA's in this raster match the table's missing values, it is.
whichNotNA <- dtTemplateRas[!is.na(vals), pixelID]
diffPixels <- setdiff(BBWA$pixelID, whichNotNA) # Integer(0) means that the data.table comes from the density rasters. So we can use density rasters as template.

# I need to extract the values of a raster stack BCR LCC PROV using the density raster for each species, where density != 0
pathData <- file.path(getwd(), "modules/birdDensityBCR_Prov_LCC/data/")
# source(file.path(getwd(), "functions/defineStudyArea.R"))
# rP <- Cache(defineStudyArea, testArea = TRUE, 
#             specificTestArea = "boreal", 
#             mapSubset = "Canada", 
#             destinationFolder = pathData, cacheId = "3a17633692bc3299") # cacheId = 3a17633692bc3299 as it is not picking up

# LCC05 <- Cache(prepInputs, url = "https://drive.google.com/open?id=19rMA800ZFsKkXx-eqBcXdR84P9MQdCLX",
#                destinationPath = pathData,
#                studyArea = rP,
#                overwrite = TRUE,
#                userTags = "objectName:LCC05")
# targetCRS <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# LCC05 <- Cache(projectInputs, LCC05, targetCRS = targetCRS, userTags = "objectName:LCC05reproj", cacheId = "e10b12018c04b906") # cacheId = e10b12018c04b906  as it is not picking up
# BCR <- Cache(prepInputs, url = "https://www.birdscanada.org/research/gislab/download/bcr_terrestrial_shape.zip",
#              targetFile = "BCR_Terrestrial_master.shp",
#              alsoExtract = "similar",
#              destinationPath = pathData,
#              studyArea = rP,
#              rasterToMatch = LCC05, overwrite = TRUE, omitArgs = c("userTags", "overwrite"),
#              userTags = c("objectName:BCR", "objectName2:newBCR")) # cacheId = e10b12018c04b906 as it is not picking up
# BCR <- Cache(rnorm, cacheId = "93222d18f73a2391")
source('/mnt/data/Micheletti/borealBirdsAndForestry/modules/birdDensityBCR_Prov_LCC/R/createBCR_PROV_LCC_Estimates.R')
BCR_Prov_LCC <- Cache(createBCR_PROV_LCC_Estimates, BCR = BCR,
                        LCC05 = LCC05, justBCRProvLCC = TRUE,
                        densityEstimates = NULL,
                        omitArgs = c("userTags"),
                        userTags = c("objectName:BCR_Prov_LCC", "script:paperPlot"))  # cacheId = e10b12018c04b906 as it is not picking up

# BCR_Prov_LCC_D <- Cache(rnorm, 1, 1, cacheId = "2e781ecc396984c3")  # cacheId = e10b12018c04b906 as it is not picking up

BCRProvLCC <- rasterStack(BCR, LCC)
# densityEstimates <- Cache(prepInputs, url = "https://drive.google.com/open?id=1SEcJdS25YkIoRMmrgGNe4-HKG90OtYjX",
#                           targetFile = "Habitat_Association_by_jurisdiction(bamddb_Apr19-2012).csv", # OBS. Not all species have densities for all combinations of LCC_PROV_BCR. Not all species are everywhere!
#                           destinationPath = pathData, fun = "data.table::fread",
#                           userTags = "objectName:densityEstimates") %>% # Also checked if it was pooled with other BCR, but not.
#   .[SPECIES %in% birdSp, c("LCC", "BCR", "PROV", "D", "D_se")]


# tableBCR_Prov_LCC_D <- merge(BCR_Prov_LCC_D$PROV_BCR_LCC, BCR_Prov_LCC_D$densityEstimates, all.x = TRUE)
# tableBCR_Prov_LCC_D$pixelID <- 1:NROW(tableBCR_Prov_LCC_D)
# # tableBCR_Prov_LCC_D <- na.omit(tableBCR_Prov_LCC_D, cols = "D")
# tableBCR_Prov_LCC_D2 <- tableBCR_Prov_LCC_D[pixelID %in% BBWA$pixelID, ]


        #########
        # PLOTS #
        #########

# PLOT1: rate of decrease for each year -- is forestry reducing? (DOES PIXEL vs TOTAL AGREE? -- only works if focal is not cummulative, 
# so it is not working, cause focal is cummulative):. Therefore we can only do it regarding the totals 
# HOW DO I KNOW the general effects of forestry are reducing in birds? If the 'rate' from one year is smaller than the previous
source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/effectsOfForestryWithTime.R')
plot1 <- effectsOfForestryWithTime(fullTableList = fullTableList)

# PLOT2: How much did habitat supply change from 1984 to the given year (cummulative rate of habitat loss) 
# Exclusively related to forest activities
source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/changeInHabitatSupplyThroughTime.R')
plot2 <- changeInHabitatSupplyThroughTime(fullTableList = fullTableList)

# PLOT4: For each species, the percentage of pixels that showed significant decline: 

# significant <- sum(significantSlope[significantSlope > 0])
# allNotNA <- sum(significantSlope[!is.na(significantSlope)])
# allNA <- sum(significantSlope[is.na(significantSlope)])
# nonSignificant <- sum(significantSlope[significantSlope == 0])
# if (!all(significant + nonSignificant == allNotNA, 
#      significant + nonSignificant + allNA == ncell(significantSlope),
#      allNotNA + allNA == ncell(significantSlope)))
#   stop("Something is not adding up. Debug")

# MAPS
# MAP 1: Map of change in disturbance: mergedFocal2011 -- only works if this is cummulative (which is)! 
    mergedFocal2011 <- raster("/mnt/data/Micheletti/borealBirdsAndForestry/modules/focalCalculation/data/mergedFocal2011-500Res250m.tif")
    library("ggplot2")
    data <- raster::as.data.frame(mergedFocal2011,
                                  xy = TRUE, na.rm = FALSE, 
                                  long = FALSE)
    names(data) <- c("x", "y", "value")
    data <- data.table::data.table(data)
    p <-  ggplot(data = data, aes(x = x, y = y)) +
      geom_tile(aes(fill = value)) +
      coord_equal() +
      # scale_fill_manual(values = discr_colors, labels = c(1:10, NA)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.border = element_blank(),
            axis.title = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      ggtitle(paste0("Cummulative change in disturbance 1984-2011"))

# MAP2: Change in habitat supply (absolute number -- realAbund2011-realAbund0) for each species
    
    
# MAP3: Change in habitat supply (proportional -- (realAbund2011-realAbund0)/realAbund0) for each species (Especially SAR?)
# PLOT3: For each species, check if the decrease is happening in areas where te birds are already not too abundant:
# Expected density for each combination of BCR_Prov_LCC (densityBIRD.tif) vs MAP3 (pixel by pixel approach): correlation test plot