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
# googledrive::drive_auth(use_oob = TRUE) # ONLY ONCE: USING RStudio Server. Doesn't work for the first time in RGui
googledrive::drive_auth(email = "tati.micheletti@gmail.com")
googledrive::drive_deauth()
SpaDES.core::setPaths(cachePath = file.path(getwd(), "cache"))
species <- c("BBWA", "BLPW", "BOCH", "BRCR", "BTNW", "CAWA", "CMWA", "CONW", "OVEN", "PISI", "RBNU", "SWTH", "TEWA", "WETA", "YRWA")

# species <- c("BBWA") # TEMPRARY!! Doing for only one species for now, just to test the workflow
onlyFinalPixelTables <- TRUE # If I only want the final full pixel tables for each sp for a reason, 
                             # this should be TRUE
doAssertions <- FALSE
shortcutToDensityTable <- TRUE

# 1. Calculate prediction area for each bird species: `birdSpecies` and `predArea`
# 2. Estimate total abundance from Peter&Diana's paper for this area * 6.25: `realAbund0`
# 1.1. Before I can calculate total abundance, I need to mask out all the non-NA pixels 
# from the expected density rasters because I have NA's in the focal rasters (i.e. lakes, 
# rivers, mountains, etc?), so we need to have exactly the same pixels as NA's in the 
# expected density and predicted rasters

# FIRST, TEST IF THE FILE ALREADY EXISTS | If not, create it
if (!onlyFinalPixelTables){
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
source(file.path(getwd(), "functions/returnBirdAbundance.R"))
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
# 4.2.2. Rate compared to abund1984: cummRate
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

# 5. Calculate the REAL AbundXXXX in each year: `realAbundXXXX` (using `realAbund0`+(`realAbund0`*`cummRateXXXX`))
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
}


######################################################################
# CALCULATE ALL THE SAME I DID FOR THE SUMMARY, but at pixel basis!  #
# NOW DO THE SAME I DID FOR THE SUMMARIZED TABLE, BUT FOR EACH PIXEL #
# FOR THAT, NEED TO DO FOR EACH BIRD                                 #
######################################################################




# Pixel by pixel basis: 
source(file.path(wd, "functions/makeBirdTable.R"))
tableFileName <- "birdsTableAbundFull"
folderForTables <- file.path(wd, "outputs/posthocAnalysis")
fullTablePixelsFile <- file.path(folderForTables, paste0(tableFileName, ".rds"))
if (file.exists(fullTablePixelsFile)){
  fullTablePixels <- readRDS(fullTablePixelsFile)
} else {
  spFullTable <- lapply(species, FUN = function(sp){
    tableFileName <- paste0("birdsTableAbund",sp)
    fullTablePixels <- Cache(makeBirdTable, species = sp, tableFileName = tableFileName,
                                     folderForTables = folderForTables,
                                     folderForPredictedRasters = file.path(wd, "modules/predictBirds/data/"),
                                     locationReturnBirdAbundanceFUN = file.path(wd, "functions/returnBirdAbundance.R"),
                                     typeOfTable = "fullTable", lightLoad = TRUE, tablePerPixel = TRUE,
                             userTags = c(paste0("objectName:fullTablePixels", sp), "typeOfTable:fullTable"))
  })
}
names(spFullTable) <- species


downloadFullTableBirdsFromURL <- FALSE # If these tables are not present, 
                                      # nor you want to calculate these, set it to TRUE

fullTableAllBirds <- lapply(X = species, function(bird){
  completeSummaryFile <- file.path(wd, "outputs/posthocAnalysis", 
                                   paste0("fullPixelTable", bird, ".rds"))
  if (file.exists(completeSummaryFile)){
    message(crayon::green("Full pixel table exists for ", bird))
    return(completeSummaryFile)
  } else {
    if (downloadFullTableBirdsFromURL) { # THIS IS FOR WHEN I NEED TO RUN IT SOMEWHERE ELSE
      message(crayon::cyan("Full pixel table doesn't exist for ", bird, ". Downloading..."))
      library("data.table")
      library("googledrive")
      fl <- Cache(drive_ls, path = as_id("1DI9k8rx4dY8P7TbMdXX9Wpv6KqXl6t4m"), 
                  recursive = TRUE, userTags = c("objectName:completeSummaryFile", 
                                                 paste0("birdSpecies:", bird)))
      browser() # Match the file id with the `bird` object. DO NOT APPLY!
      # fl <- fl[match(bird)]
        destPath <- file.path(getwd(), "outputs/posthocAnalysis", fl$drive_resource$name)
        url <- paste0("https://drive.google.com/open?id=", eachTable$drive_resource$id)
        tablePath <- reproducible::preProcess(url = url, destinationPath = destPath,
                                              targetFile = eachTable$drive_resource$name)
      browser() # Check 
      # names(completeSummaryFile) <- 
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
      # Now real density
      spDensityTable <- lapply(X = species, FUN = function(sp){
        fl <- usefun::grepMulti(x = list.files(path = maskedDensityRasFolder,
                                               full.names = TRUE), patterns = c("density", sp,".tif"))
        fullDensityTable <- Cache(returnBirdAbundance, filepath = fl, type = "density",
                                                fullTableFilename = paste0("densityFullTable", sp, ".rds"),
                                                summarizedTableFileName = paste0("summarizedTableFileName", sp),
                                                whichToLoad = "fullTable", tablePerPixel = TRUE,
                                  userTags = c(paste0("objectName:fullDensityTable", sp), "typeOfTable:fullTable"))
      })
      names(spDensityTable) <- species
      
      fullTableList <- usefun::cbindFromList(list(fullTableList, spDensityTable[[bird]])) # BEFORE MERGING CHECK IF BOTH TABLES HAVE THE SAME VALUES FOR TOTAL PIXELS!
      names(fullTableList)[names(fullTableList) == "density"] <- "realAbund0"
      # 5.2 calculating the real abundance (based on the original densities and the rate of loss)
      cummRates <- usefun::grepMulti(x = names(fullTableList), patterns = "cumm")
      lapply(X = cummRates, FUN = function(cum){
        newColName <- paste0("realAbund", 
                             usefun::substrBoth(strng = cum, 
                                                howManyCharacters = 4, 
                                                fromEnd = TRUE))
        fullTableList[, c(newColName) := realAbund0 + (get(cum)*realAbund0)]
      })
      saveRDS(fullTableList, file = completeSummaryFile)
      return(completeSummaryFile) 
    }
  }
})
names(fullTableAllBirds) <- species


# HERE


# To correct for LCC previous to 2005 that we don't know what was, we can use the minimum and the maximum values of 
# each one of the forest cover types that were harvested until 2011 for each pixel.
# 1. I need to extract the values of a raster stack BCR LCC PROV using the density raster for each species
pathData <- file.path(getwd(), "modules/birdDensityBCR_Prov_LCC/data/")
# Here I need the table table BCR, Prov, LCC, ExpD so I can check the min, max and average for 
# each combination of BCR + Prov for forested LCC (classes: 1:15)
if (!shortcutToDensityTable){
  source(file.path(getwd(), 'outputs/posthocAnalysis/makeBCRandLCC.R'))
  BCRLCC05 <- Cache(makeBCRandLCC, pathData = pathData)
  densityTable <- Cache(prepInputs, url = "https://drive.google.com/open?id=1SEcJdS25YkIoRMmrgGNe4-HKG90OtYjX",
                            targetFile = densityEstimatesFileName, # OBS. Not all species have densities for all combinations of LCC_PROV_BCR. Not all species are everywhere!
                            destinationPath = asPath(pathData), fun = "data.table::fread",
                            userTags = "objectName:densityEstimates", overwrite = TRUE) %>% # Also checked if it was pooled with other BCR, but not.
    .[SPECIES %in% species,]
  source(file.path(getwd(), 'modules/birdDensityBCR_Prov_LCC/R/createBCR_PROV_LCC_Estimates.R'))
  BCR_Prov_LCC <- Cache(createBCR_PROV_LCC_Estimates, BCR = BCRLCC05$BCR,
                          LCC05 = BCRLCC05$LCC05, justBCRProvLCC = FALSE,
                          densityEstimates = densityTable,
                          omitArgs = c("userTags"),
  userTags = c("objectName:BCR_Prov_LCC", "script:paperPlot"))
} else {
  densityTable <- Cache(prepInputs, url = "https://drive.google.com/open?id=10lQSxK-DQ6CovtOwSo9w-ggr2HfsWrds", 
                             destinationPath = file.path(wd, "outputs/posthocAnalysis/"), 
                             targetFile = "originalDensityTable.rds", fun = "readRDS")
  BCR_Prov_LCC <- Cache(prepInputs, url = "https://drive.google.com/open?id=1-ZXdU1DGzQOdOTrqHDd2HsPzdWOp5lmZ", 
                        destinationPath = file.path(wd, "outputs/posthocAnalysis/"), 
                        targetFile = "BCRProvLCCPixel.rds", fun = "readRDS", overwrite = TRUE)
}

# For each species...
pixelTablesWithUncertaintyPre05 <- lapply(X = species, FUN = function(BIRD){
  # Exclude uninportant columns 
  densityTableSub <- densityTable[SPECIES == BIRD, c("LCC", "BCR", "PROV", "D", "D_se")]
  # Merge to get the Density per BCR_Prov_LCC per pixel
  densityTableSubPixelID <- Cache(merge, BCR_Prov_LCC, densityTableSub, by = c("BCR", "PROV", "LCC"), all.x = TRUE)
  # Get the min, max and average D for each combination of BCR and PROV, among all forested LCC classes (1:15)
  densityTableSubPixelID[LCC %in% 1:15, c("minD", "maxD", "aveD") := list(min(D, na.rm = TRUE), 
                                                                           max(D, na.rm = TRUE), 
                                                                           mean(D, na.rm = TRUE)), by = c("BCR", "PROV")]
  
  # Merge now with the fullTableAllBirds of each bird to add min, max and average to the full table
  fullTableUncertain <- merge(readRDS(fullTableAllBirds[[BIRD]]),
                          densityTableSubPixelID, by = "pixelID", all.x = TRUE)
  
})


        #########
        # PLOTS #
        #########

# PLOT1: rate of decrease for each year -- is forestry reducing? (DOES PIXEL vs TOTAL AGREE? -- only works if focal is not cummulative, 
# so it is not working, cause focal is cummulative):. Therefore we can only do it regarding the totals 
# HOW DO I KNOW the general effects of forestry are reducing in birds? If the 'rate' from one year is smaller than the previous
source(file.path(getwd(), "functions/effectsOfForestryWithTime.R"))
plot1 <- effectsOfForestryWithTime(fullTableList = fullTableList)

# PLOT2: How much did habitat supply change from 1984 to the given year (cummulative rate of habitat loss) 
# Exclusively related to forest activities
source(file.path(getwd(), "functions/changeInHabitatSupplyThroughTime.R"))
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
