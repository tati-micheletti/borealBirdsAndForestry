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
maskedDensityRasFolder <- file.path(wd, "outputs/posthocAnalysis/maskedDensityRas")
originalDensFolder <- file.path(wd, "modules/birdDensityBCR_Prov_LCC/data")
source(file.path(getwd(), "functions/returnBirdAbundance.R"))


# googledrive::drive_auth(use_oob = TRUE) # ONLY ONCE: USING RStudio Server. Doesn't work for the first time in RGui
googledrive::drive_auth(email = "tati.micheletti@gmail.com")
SpaDES.core::setPaths(cachePath = file.path(getwd(), "cache"))
species <- c("BBWA", "BLPW", "BOCH", "BRCR", "BTNW", "CAWA", "CMWA", "CONW", "OVEN", "PISI", "RBNU", "SWTH", "TEWA", "WETA", "YRWA")
spatialScale <- 100 # 500m DONE! 
doAssertions <- TRUE # Should NOT be turned off
freeUpMem <- FALSE
lightLoadFinalTable <- FALSE

# 1. Calculate prediction area for each bird species: `birdSpecies` and `predArea`
# 2. Estimate total abundance from Peter&Diana's paper for this area * 6.25: `realAbund0`
# 1.1. Before I can calculate total abundance, I need to mask out all the non-NA pixels 
# from the expected density rasters because I have NA's in the focal rasters (i.e. lakes, 
# rivers, mountains, etc?), so we need to have exactly the same pixels as NA's in the 
# expected density and predicted rasters

# Pixel by pixel basis: 
source(file.path(wd, "functions/makeBirdTable.R"))
tableFileName <- "birdsTableAbundFull"
folderForTables <- checkPath(file.path(wd, "outputs/posthocAnalysis", paste0(spatialScale, "m")), create = TRUE)
fullTablePixelsFile <- file.path(folderForTables, paste0(tableFileName, ".rds")) 
if (file.exists(fullTablePixelsFile)){ # THIS COULD BE DEPRECATED. ITS SUPER MEMORY CONSUMING. 
  # ITS BETTER TO JOIN ALL BIRDS AFTERWARDS 
  message(crayon::green("Full table of predictions exist for all birds. Loading..."))
  fullTablePixels <- readRDS(fullTablePixelsFile)
} else {
  message(crayon::yellow("Full table of predictions doesn't exist for all birds. Creating..."))
  # Make a density table. This has the original DENSITY, per pixel, taken out straight from the density/predicted rasters
  # CALCULATED/PREDICTED DENSITY, NOT THE ORIGINAL Solymos/Stralberg density!!
  fullTablePixels <- lapply(species, FUN = function(sp){
    tableFileName <- paste0("birdsTableAbund",sp, spatialScale, "m")
    fullTablePixels <- Cache(makeBirdTable, species = sp, tableFileName = tableFileName,
                                     folderForTables = folderForTables,
                                     spatialScale = spatialScale,
                                     folderForPredictedRasters = file.path(wd, "modules/predictBirds/data/"),
                                     locationReturnBirdAbundanceFUN = file.path(wd, "functions/returnBirdAbundance.R"),
                                     typeOfTable = "fullTable", lightLoad = FALSE, tablePerPixel = TRUE,
                             onlyNA = TRUE,
                             userTags = c(paste0("objectName:fullTablePixels", sp), "typeOfTable:fullTable"),
                             omitArgs = c("overwriteInternals", "useCache", "userTags", "destinationPath"))
  })
}
names(fullTablePixels) <- species
downloadFullTableBirdsFromURL <- FALSE # If these tables are not present and 
                                       #  you don't want to calculate these (i.e. download the originally calculated from GoogleDrive), 
                                       # set it to TRUE ==> Not yet fully implemented

source(file.path(getwd(), 'outputs/posthocAnalysis/makeBCRandLCC.R'))
pathData <- file.path(getwd(), "modules/birdDensityBCR_Prov_LCC/data/")
BCRLCC05 <- Cache(makeBCRandLCC, pathData = pathData, userTags = c("objectName:BCRLCC05", "script:paperPlots"), 
                  overwrite = TRUE, omitArgs = c("overwrite", "userTags", "useCache")) # rasterToMatch = BCRLCC05$LCC05

fullTableAllBirds <- lapply(X = species, function(bird){
  completeSummaryFile <- file.path(folderForTables,
                                   paste0("fullPixelTable", bird, paste0(spatialScale, "m"), ".rds"))
  if (file.exists(completeSummaryFile)){
    message(crayon::green("Full pixel table (predictions + rates) exists for ", bird))
    return(completeSummaryFile)
  } else {
    if (downloadFullTableBirdsFromURL) { # THIS IS FOR WHEN I NEED TO RUN IT SOMEWHERE ELSE
      message(crayon::cyan("Full pixel table (predictions + rates)  doesn't exist for ", bird, ". Downloading..."))
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
      message(crayon::yellow(paste0("Full pixel table doesn't exist for ", bird, ". Creating...")))
      
      library("data.table")
      # Loading `realAbund0` that comes from the densityBIRD.tif.
      # These seem to be the original density tables (from Solymos Stralberg) 08APR20 
      # It loads already as abundance, but the name stays as density until step 5.
dfullTpath <- checkPath(file.path(dirname(maskedDensityRasFolder), "densityFullTables"), create = TRUE)
        fl <- usefun::grepMulti(x = list.files(path = maskedDensityRasFolder,
                                               full.names = TRUE), patterns = c("density", bird,".tif"))
        fullDensityTable <- Cache(returnBirdAbundance, 
                                  filepath = fl, 
                                  type = "density",
                                  fullTableFilename = file.path(dfullTpath, paste0("densityFullTable", bird, ".rds")), 
                                  summarizedTableFileName = paste0("summarizedTableFileName", bird),
                                  whichToLoad = "fullTable", 
                                  tablePerPixel = TRUE, 
                                  onlyNA = TRUE,
                                  userTags = c(paste0("objectName:fullDensityTable", bird), 
                                               paste0("species:", bird), "script:paperPlots",
                                               "typeOfTable:fullTable"),
                                  omitArgs = "useCache")
        setkey(fullDensityTable, "pixelID")
      # 4.2 RATE YEARS 1984 - 2011 PER BIRD
      # 4.2.1 Rate per year: rate
      fullTableList <- readRDS(fullTablePixels[[bird]])
      dcastedTable <- dcast(data = fullTableList, formula = species + pixelID ~ year, value.var = "density")
      ys <- usefun::substrBoth(unique(fullTableList$year), howManyCharacters = 4, fromEnd = TRUE)
      newNames <- c(names(dcastedTable)[1:2], paste0("abund", ys))
      names(dcastedTable) <- newNames
      fullTableList <- dcastedTable
      abundNames <- usefun::grepMulti(names(fullTableList), patterns = c("abund"))
      envir <- environment()
      invisible(lapply(2:length(abundNames), function(i){
        t1 <- Sys.time()
        yearRef <- usefun::substrBoth(strng = abundNames[i],
                                      howManyCharacters = 4,
                                      fromEnd = TRUE)
        thisYearsRate <- (fullTableList[,abundNames[i], with = FALSE]-
                            fullTableList[,abundNames[i-1], with = FALSE])/
          fullTableList[,abundNames[i-1], with = FALSE]
        names(thisYearsRate) <- paste0("rate", yearRef)
        assign("fullTableList", cbind(fullTableList, thisYearsRate), envir = envir)
        t2 <- Sys.time()
        message(crayon::green(paste0("Yearly rates finished for ", 
                                     usefun::substrBoth(abundNames[i],
                                                        howManyCharacters = 4, fromEnd = TRUE), 
                                     ". Elapsed time: " , t2-t1)))
      }))
      
      # 4.2.2. Rate compared to abund1984: cummulativeRate
      invisible(lapply(2:length(abundNames), function(i){
        t1 <- Sys.time()
        yearRef <- usefun::substrBoth(strng = abundNames[i],
                                      howManyCharacters = 4,
                                      fromEnd = TRUE)
        thisYearsRate <- (fullTableList[,abundNames[i], with = FALSE]-
                            fullTableList[,"abund1984"])/
          fullTableList[,"abund1984"]
        names(thisYearsRate) <- paste0("cummRate", yearRef)
        assign("fullTableList", cbind(fullTableList, thisYearsRate), envir = envir)
        t2 <- Sys.time()
        message(crayon::yellow(paste0("Yearly cummulative rates finished for ", 
                                      usefun::substrBoth(abundNames[i],
                                                         howManyCharacters = 4, fromEnd = TRUE), 
                                      ". Elapsed time: " , t2-t1)))
      })
      )
      setkey(fullTableList, "pixelID")
      # 5. Calculate the AbundXXXX in each year: 
      # `realAbundXXXX` (using `realAbund0`+(`realAbund0`*`cummRateXXXX`)). 
      # THis is the most likely abundance if we consider that the same type of 
      # forest regenerates after logging. That's why I am keeping it here
      
      # 5.1 Attach the `realAbund0` to the full table
      fullTableList <- merge(fullTableList, fullDensityTable, by = c("pixelID", "species"))
      names(fullTableList)[names(fullTableList) == "abundance"] <- "realAbund0"  
      # Inside returnBirdAbundance/areaAndAbundance, we are already multiplying by area 
      # (i.e. converting density into abundance) using prod(raster::res(birdRas))/10000
      
      # 5.2 calculating the real abundance (based on the original densities and the rate of loss)
      # Will be re-done is a next step, as it needs to be done considering uncertainty regarding 
      # which type of forest was harvested pre-2005
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

# To correct for LCC previous to 2005 that we don't know what type of forest was, we 
# can use the minimum and the maximum values of 
# each one of the forest cover types that were harvested until 2011 for each pixel.
# 1. I need to extract the values of a raster stack BCR LCC PROV using the density raster 
# for each species
# Here I need the table table BCR, Prov, LCC, ExpD so I can check the min, max and average for 
# each combination of BCR + Prov for forested LCC (classes: 1:15)

# For each species...
pixelTablesWithUncertaintyPre05 <- lapply(X = species, FUN = function(BIRD){
  finalFullTablePath <- file.path(folderForTables, 
                                  paste0("finalFullTable", BIRD, 
                                         spatialScale,"m.rds"))
  if (!file.exists(finalFullTablePath)){
  message(crayon::red(paste0("finalFullTable for ", BIRD, " does not exist. Creating...")))
    densityTable <- Cache(prepInputs, 
                          url = "https://drive.google.com/open?id=1SEcJdS25YkIoRMmrgGNe4-HKG90OtYjX",
                          targetFile = "Habitat_Association_by_jurisdiction(bamddb_Apr19-2012).csv", 
# OBS. Not all species have densities for all combinations of LCC_PROV_BCR. 
# Not all species are everywhere!
# I need this table because I need all possible combinations of the bird density and 
# a cover class, not only the predicted one by Solymos Stralberg (that one is the 
# "most likely scenario", though)
                          destinationPath = asPath(pathData), fun = "data.table::fread",
                          userTags = c("objectName:densityEstimates", "script:paperPlots")) 
# Also checked if it was pooled with other BCR, but not.
    densityTable <- densityTable[SPECIES %in% species,]
    source(file.path(getwd(), 'functions/createBCR_PROV_LCC_EstimatesPosthoc.R'))
    BCR_Prov_LCC <- Cache(createBCR_PROV_LCC_EstimatesPosthoc, BCR = BCRLCC05$BCR,
                          LCC05 = BCRLCC05$LCC05, justBCRProvLCC = TRUE, 
                          pathToDSave = maskedDensityRasFolder, 
                          densityMap = file.path(maskedDensityRasFolder, "densityBBWA.tif"), 
# Can be used for all species as it is a template for RTM! 
                          omitArgs = c("userTags", "useCache"),
                          userTags = c("objectName:BCR_Prov_LCC", "script:paperPlot"))

    # Subset BCR_Prov_LCC to the pixels where we actually have any data:
    BIRDfullTable <- readRDS(fullTableAllBirds[[BIRD]])
    setkey(BIRDfullTable, "pixelID")
    pixelsWithData <- BIRDfullTable[["pixelID"]]
    
    # Exclude uninportant columns 
    densityTableSub <- densityTable[SPECIES == BIRD, c("LCC", "BCR", "PROV", "D", "D_se")]
    # Merge to get the Density per BCR_Prov_LCC per pixel
    densityTableSubPixelID <- Cache(merge, BCR_Prov_LCC, densityTableSub, 
                                    by = c("BCR", "PROV", "LCC"), 
                                    all.x = TRUE, omitArgs = c("useCache"), 
                                    userTags = c("script:paperPlots", 
                                                 "objectName:densityTableSubPixelID"))
    setkey(densityTableSubPixelID, "pixelID")
    # Assertion regarding the original density form Solymos&Stralber 
    # being the same as realAbund0/mostLikelyAbund and abundance from the full tables
    if (doAssertions){ 
      # For BLPW (and maybe other birds?) a few pixels are not agreeing 
      # (Abund and realAbund0|originalDMaps).
      # Anywhere where I use Abund, I should be using realAbund0 instead 
      # until I figure this one out --> [08APR2020] we detected a problem in postProcess a few
      # weeks ago. Should be fixed now, but means I should probably re-run all these analysis at 
      # some point].
      # (Should NOT turn off this assertion because I fixed it up in here. 
      # Exchanged Abund for realAbund0. If the assertion fails, I'm in shit...)! [19Sept19]
      # 1. originalDMaps <- DENSITY MAPS * 6.25
      originalDens <- raster(file.path(wd, 
                                       paste0("outputs/posthocAnalysis/maskedDensityRas/density", 
                                              BIRD, ".tif")))
      originalDensPostProc <- Cache(reproducible::postProcess, 
                                    originalDens, rasterToMatch = NULL, #BCRLCC05$LCC05, 
                                    userTags = c("script:paperPlots", 
                                                 "goal:snappingDensityRas", 
                                                 paste0("species:", BIRD)))
      testTable <- data.table(pixelID = 1:raster::ncell(originalDensPostProc), 
                              originalDMaps = getValues(originalDensPostProc)*6.25)
      # 2. BIRDfullTableD <- Abundance COMING FROM THIS TABLE "BIRDfullTable" (realAbund0)
      BIRDfullTableD <- BIRDfullTable[, c("pixelID", "realAbund0")]
      # 3. densityTableSubPixelIDAbund <- (future Abund) current D from "densityTableSubPixelIDsimp"
      densityTableSubPixelIDAbund <- densityTableSubPixelID[, c("pixelID", "D")]
      
      # Then something like:
      testTable <- merge(testTable, densityTableSubPixelIDAbund, by = "pixelID", all.x = TRUE)
      testTable <- merge(testTable, BIRDfullTableD, by = "pixelID", all.x = TRUE)
      testTableNoNA <- na.omit(testTable)
      ok <- all(all(testTableNoNA[, round(originalDMaps, 5) == round(D, 5)]),
                all(testTableNoNA[, round(D, 5) == round(realAbund0, 5)]))
      if (!ok) message(crayon::red(
        paste0("The density from one of the sources ",
        "(probably D/Abund, coming from the created LCC_PROV_BCR) does not match the others.",
        "A fix will be applied but this should be debugged ASAP.")))
      # Here is my plan: realAbund0 is working fine (in accordance with 
      # the original density maps). So here I replace
      # D with realAbund0. This way I can potentially fix this problem. 
      # Should be revised later, though [19Sep19]
      # TODO
      realAbund0 <- BIRDfullTable[, c("pixelID", "realAbund0")]
      densityTableSubPixelID <- merge(densityTableSubPixelID, realAbund0, by = "pixelID", all.x = TRUE)
      # Does the number of pixels with values match D and realAbund0? 
      # If so, it should be safe to replace one with the other
      ok <- length(!is.na(densityTableSubPixelID$D)) == length(!is.na(densityTableSubPixelID$realAbund0))
      if (!ok) stop("For some wicked reason, the fix applied did not work. 
                    NA's in D don't match realAbund0. 
                    Good luck fixing it! I'm out!")
      message(crayon::green("Ufff! The fix worked! 
                            Fine for now, but please debug it ASAP."))
      densityTableSubPixelID[, D := NULL]
      names(densityTableSubPixelID)[names(densityTableSubPixelID) == "realAbund0"] <- "D"
      # Divide the new D for 6.25 as it comes from the realAbund0, which is already 
      # in abundance form (density*6.25).
      densityTableSubPixelID$D <- densityTableSubPixelID$D/6.25
    }
  
    # The densities (D) of pixels that have no 'se' (i.e. D_se == NA) are likely just artifacts. 
    # We should set these to 0 (i.e. the birds are technically not there. 
    # Converting to NA would be a possibility, but I think 0 is better as we know it is 
    # being predicted there (no holes on the map) 
    # 1. Checked the range of values that have NA in D_se but something in D so I can make a 
    # cut in a bin that makes sense. Here we found out that we have a few values between 
    # 0.005 and 0.0052. 
    # These might not be artifacts so I will keep these even though their D_se is NA. 
    # I will cut them off in 0.00000001
    # DwhereSEisNA <- densityTableSubPixelID[is.na(D_se), D]
    # br <- seq(0, 0.006, by = 0.001)
    # ranges <- paste(head(br, -1), br[-1], sep = " - ")
    # freq <- hist(DwhereSEisNA, breaks = br, include.lowest=TRUE, plot = TRUE)
    # data.frame(range = ranges, frequency = freq$counts)
    # onlyLower <- DwhereSEisNA[DwhereSEisNA < 0.002 & !is.na(DwhereSEisNA)]
    # max(onlyLower) # 1.28e-08
    densityTableSubPixelID[D < 0.0000001 & is.na(D_se), D := 0]
    
    # Get the min, max and average D for each combination of BCR and PROV, 
    # among all forested LCC classes (1:15)
    # maybe need to include LCC classes 20 and 25
    densityTableSubPixelID[, validD := ifelse(!is.na(D) & D > 0, D, NA)] 
    # As zero's should not be considered for the interval, as they are derived from artifacts!
    densityTableSubPixelID[LCC %in% c(1:15), c("minD", "maxD") := list(min(validD, na.rm = TRUE), 
                                                                       max(validD, na.rm = TRUE)),
                           by = c("BCR", "PROV")] 
    # When we have NA's in minD and maxD means we have LCC that is not forest.
    # Here we fix the min and max for non-forests (we set both to D, so no variation), 
    # remembering this should ONLY happen for YYYY =< 2005
    densityTableSubPixelID[is.na(minD) | is.na(maxD), c("minD", "maxD") := validD] 
    # If Abund is NA, min and max need to be NA as well (i.e. abund is NA 
    # for pixels without prediction, so it can't have min or max!)
    densityTableSubPixelID[is.na(validD), c("minD", "maxD") := NA] 
    
    if (freeUpMem){
      rm(BCR_Prov_LCC, envir = .GlobalEnv)
      rm(densityTable, envir = .GlobalEnv)
      gc()
    }
    
    # simplify tables so I don't end up with monsters
    densityTableSubPixelIDsimp <- densityTableSubPixelID[, c("pixelID", "D_se", 
                                                             "D", "minD", "maxD")]
    rm(densityTableSubPixelID); gc()
    # 1. Multiply density by 6.25 to convert to Abundance (D, D_se, minD, maxD)
    # Could exclude aveD... doesn't make ecological sense! If anything, the closest 
    # to be correct one is 'realAbund'
    # 2. Keep D and D_se so we can have the "most likely scenario" and uncertainty!
    if (doAssertions){
      ok <- all(names(densityTableSubPixelIDsimp) == c("pixelID", "D_se", "D", "minD", "maxD"))
      if (!ok) stop("Names of densityTableSubPixelIDsimp are not matching the order, please debug!")
    }
    DtoAbund <- c("D_se", "D", "minD", "maxD")
    densityTableSubPixelIDsimp[, (DtoAbund) := lapply(.SD, function(x)
      x * 6.25), .SDcols = DtoAbund] # This D is really density 
    names(densityTableSubPixelIDsimp) <- c("pixelID", "Abund_se", "Abund", "minAbund", "maxAbund")
    
    # 3. Identify which pixels changed before 2005 (just subtract cummRate2005 from 
    # cummRate1985 and anything other than 0) and simplify table
    # assertion regarding the data coming from the species in question
    if (doAssertions){
      if (BIRD != unique(BIRDfullTable[,species])) 
        stop("Bird species specified and species in the bird table are not the same. Debug.")
    }
    
    # simplify table
    colsToDrop <- names(BIRDfullTable)[!names(BIRDfullTable) %in% c("pixelID", 
                                                                    paste0("realAbund", 
                                                                           c(0, 1984:2011)),
                                                                    grepMulti(x = names(BIRDfullTable), 
                                                                              patterns = "cum"))]
    BIRDfullTable[, (colsToDrop) := NULL]
    # 4. Create a new table: with realAbundYEARmin realAbundYEARmax, 
    # where for pixels that didn't change, 
    # these two columns have the same value (the original realAbundYEAR)
    # 4.1 Join the table with Abund, Abund_se, minAbund, maxAbund
    BIRDfullTable <- merge(BIRDfullTable,
                           densityTableSubPixelIDsimp, by = "pixelID", all.x = TRUE)
    
    # Identify which pixels changed before 2005
    BIRDfullTable[, pixelChanged := ifelse(cummRate1985-cummRate2005 != 0, TRUE, FALSE)] 
    #all(LCC %in% 1:15)
    source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/percentOfAreaAffectedByDisturbance.R')
    percChange <- percentOfAreaAffectedByDisturbance(tableWithChange = BIRDfullTable, 
                                                     logicalColChange = "pixelChanged")
    message(crayon::cyan(paste0("Percent area that is affected by ANY change for ", BIRD, ": ", 
                                100*round(as.numeric(percChange), 2), "%")))
    saveRDS(percChange, file.path(folderForTables, 
                                  paste0("percentChange", BIRD, spatialScale,".rds")))
    
    # 4.2 Calculate realAbundYYYY OR minrealAbundYYYY for years 1985-2005
    cummRates <- usefun::grepMulti(x = names(BIRDfullTable), patterns = "cumm")
    lapply(X = cummRates, FUN = function(cum){
      YEAR <- usefun::substrBoth(strng = cum, 
                                 howManyCharacters = 4, 
                                 fromEnd = TRUE)
      newColName <- paste0("realAbund", 
                           YEAR)
      if (as.numeric(YEAR) < 2006){
        BIRDfullTable[, c(paste0("min", newColName)) := ifelse(pixelChanged == TRUE, 
                                                               minAbund + (get(cum)*minAbund), 
                                                               Abund + (get(cum)*Abund))]
        BIRDfullTable[, c(paste0("max", newColName)) := ifelse(pixelChanged == TRUE, 
                                                               maxAbund + (get(cum)*maxAbund), 
                                                               Abund + (get(cum)*Abund))]
      } else {
        BIRDfullTable[, c(paste0("min", newColName), 
                          paste0("max", newColName)) := realAbund0 + (get(cum)*realAbund0)]
      }
    })
    # 4.3 Simplify and save new table...
    saveRDS(BIRDfullTable, finalFullTablePath)
  } else {
    if (lightLoadFinalTable){
      message(crayon::green(paste0("finalFullTable (predictions + rates + real",
                                   " abundances per year)  for ", BIRD, 
                                   " exists. Returning path...")))
      return(finalFullTablePath)
    } else {
      message(crayon::green(paste0("finalFullTable (predictions + rates + real ",
                                   "abundances per year)  for ", BIRD, 
                                   " exists. Returning table...")))
      return(readRDS(finalFullTablePath))
    }
  }
})
names(pixelTablesWithUncertaintyPre05) <- species

# 21NOV19: Some @241,000 pixels have Abund == 0 and no min or max. I think this is also an artifact. 
# I checked all columns and none has the max value at more than 1.1875e-08. So I decided to convert 
# all values of *Abund* (i.e. minrealAbund, realAbund, Abund_se) to zero. This avoid NA's on the 
# final table.

# Select the columns that have Abund in the name
toConvertToZero <- usefun::grepMulti(x = names(pixelTablesWithUncertaintyPre05[[1]]), 
                                     patterns = "Abund") 
# can be 1 because it is a template just for name 
finalPixelTableList <- lapply(pixelTablesWithUncertaintyPre05, FUN = function(eachBIRD){
  eachBIRD[Abund == 0, paste(toConvertToZero) := 0]
})

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# MANAGED FOREST VS NON-MANAGED FOREST #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Here is where we can do any sort of summary by polygon. After this point, I go for summarizing all the tables
# 1. Load a shapefile and extract the ID by pixel (i.e. have a dt with pixelID and a new column region)
managedForest <- Cache(prepInputs, url = "https://drive.google.com/open?id=1tgqn8FajD1iSj0aECONGhFzwInau0-q8",
                            targetFile = "NIR2016_MF.shp", archive = "NIR2016_MF.zip",
                            alsoExtract = "similar",
                            studyArea = BCRLCC05$BCR, rasterToMatch = BCRLCC05$LCC05, 
                            overwrite = TRUE, omitArgs = c("overwrite"),
                            destinationPath = file.path(getwd(), "inputs"), 
                       userTags = "objectName:managedForest")

# Need to rasterize the shapefile for getting pixelID
managedForestSF <- sf::st_as_sf(x = managedForest)
managedForestSF$value <- c(1, 2) # Length of the dataset
# MANAGED = 1
# NON-MANAGED = 2
managedForestRAS <- fasterize::fasterize(sf = managedForestSF, 
                                         raster = BCRLCC05$LCC05, field = "value")
plot(managedForestRAS)
managedForestDT <- data.table::data.table(pixelID = 1:ncell(managedForestRAS), 
                                          value = raster::getValues(managedForestRAS))

# Now I can put together the managedForest raster and the birds table
finalPixelTableList <- lapply(names(finalPixelTableList), function(sp){
  finalPixelTableList[[sp]] <- merge(finalPixelTableList[[sp]], managedForestDT, by = "pixelID")
  tbName <- file.path(dirname(maskedDensityRasFolder), 
                      paste0("finalPixelTable_forSummary_", sp,".rds"))
  saveRDS(finalPixelTableList[[sp]], file = tbName)
  return(tbName)
})
names(finalPixelTableList) <- species

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# MANAGED FOREST FOR EACH PROVINCE #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

# Here is where we can do any sort of summary by polygon. After this point, I go for summarizing all the tables
# 1. Load a shapefile and extract the ID by pixel (i.e. have a dt with pixelID and a new column region)
managedForest <- Cache(prepInputs, url = "https://drive.google.com/open?id=1tgqn8FajD1iSj0aECONGhFzwInau0-q8",
                       targetFile = "NIR2016_MF.shp", archive = "NIR2016_MF.zip",
                       alsoExtract = "similar",
                       studyArea = BCRLCC05$BCR, rasterToMatch = BCRLCC05$LCC05, 
                       overwrite = TRUE, omitArgs = c("overwrite"),
                       destinationPath = file.path(getwd(), "inputs"), 
                       userTags = "objectName:managedForest")

managedForest <- managedForest[managedForest$ManagedFor == "Managed Forest",]

# Creating codes to rasterize BCR province so we can decide if we do the comparison at 
# Province, BCR or both levels

BCR <- BCRLCC05[["BCR"]]
library(data.table)
# Improve speed, drop columns that we dont need
colsToDrop <- c("BCRNAME", "COUNTRY", "REGION", "WATER", "Shape_Leng", "Shape_Area", "Id")
# The BCR shapefile has some sort of water, non-water polygons. Need to drop those water ones before doing this!
# WATER == 1 needs to be removed, and so does country == USA
BCR <- BCR[BCR$WATER == 3 & BCR$COUNTRY == "CANADA" ,
           !(names(BCR) %in% colsToDrop)]
BCRdt <- data.table(BCR@data)

BCRdt$BCR_PROV <- 1:NROW(BCRdt)
provDT <- data.table(PROVINCE_S = unique(BCRdt$PROVINCE_S))
provDT$PROVINCE <- 1:NROW(provDT)
BCRdt <- merge(BCRdt, provDT, by = "PROVINCE_S")
# Check the length of each possible variable to use for comparisons  
length(unique(BCRdt$BCR)) # 10
length(unique(BCRdt$PROVINCE)) # 10
length(unique(BCRdt$PROVINCE_S)) # 10
length(unique(BCRdt$BCR_PROV)) # 40

knitr::kable(BCRdt)

# Need to rasterize the shapefile for getting pixelID,
# but first need to add BCR_PROV and PROVINCE TO SHAPEFILE (BCR is there already)
BCRdtOriginal <- data.table(BCR@data)
BCRdtOriginalData <- merge(BCRdtOriginal, BCRdt, by = c("BCR", "PROVINCE_S"))
BCR$PROVINCE <- BCRdtOriginalData$PROVINCE
BCR$BCR_PROV <- BCRdtOriginalData$BCR_PROV

# Function to extract the values to a table
source(file.path(getwd(), 'functions/extractValuesToTable.R'))
rasProv <- Cache(extractValuesToTable, destinationPath = file.path(getwd(), "inputs"), 
                                shape = BCR, extractFrom = managedForest,
                                templateRas = BCRLCC05$LCC05, 
                                field = "PROVINCE")
rasBCR <- Cache(extractValuesToTable, destinationPath = file.path(getwd(), "inputs"),
                               shape = BCR, extractFrom = managedForest, 
                               templateRas = BCRLCC05$LCC05, 
                               field = "BCR")
rasProvBCR <- Cache(extractValuesToTable, destinationPath = file.path(getwd(), "inputs"),
                                   shape = BCR, extractFrom = managedForest,
                                   templateRas = BCRLCC05$LCC05, 
                                   field = "BCR_PROV")
# Now I have a data.table of pixelID, merge all and then merge with finalPixelTableList
managedForestProvBCR <- merge(rasProv, rasBCR, by = "pixelID")
managedForestProvBCR <- merge(managedForestProvBCR, rasProvBCR, by = "pixelID")
names(managedForestProvBCR) <- c("pixelID", "PROV", "BCR", "PROV_BCR")

# Now I can put together the managedForest raster and the birds table
finalPixelTableListBCRPROV <- lapply(names(finalPixelTableList), function(sp){
  finalPixelTableList[[sp]] <- merge(finalPixelTableList[[sp]], 
                                     managedForestProvBCR, by = "pixelID")
  # REMOVE NA's --> NA's in any of the columns == non-managed forests!
  finalPixelTableList[[sp]] <- na.omit(finalPixelTableList[[sp]])
  tbName <- file.path(dirname(maskedDensityRasFolder), 
                      paste0("finalPixTab_BCRPROV_forSummary_", sp,".rds"))
  saveRDS(finalPixelTableList[[sp]], file = tbName)
  return(tbName)
})
names(finalPixelTableListBCRPROV) <- species

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# MANAGED FOREST FOR EACH PROVINCE #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TABLE WITH MANAGED AND UNMAGAED FORESTS
# SHORTCUT:: run from here on only
finalPixelTableList <- lapply(species, function(sp){
  tbName <- file.path(dirname(maskedDensityRasFolder),
                      paste0("finalPixelTable_forSummary_", sp,".rds"))
  return(tbName)
})
names(finalPixelTableList) <- species
 
# TABLE WITH MANAGED FOREST ONLY
finalPixelTableListBCRPROV <- lapply(species, function(sp){
  tbName <- file.path(dirname(maskedDensityRasFolder),
                      paste0("finalPixTab_BCRPROV_forSummary_", sp,".rds"))
  return(tbName)
})
names(finalPixelTableListBCRPROV) <- species


# CREATING FINAL TABLES

borealTable <- makesummarizedTableFromPixels(tabName = "summarizedTableFromPixels.rds", 
                                             maskedDensityRasFolder = maskedDensityRasFolder, 
                                             column = "value", finalPixelTableList = finalPixelTableList,
                                             species = species)
bcrTable <- makesummarizedTableFromPixels(tabName = "summarizedTableFromPixelsBCR.rds", 
                                          maskedDensityRasFolder = maskedDensityRasFolder, 
                                          column = "BCR",  finalPixelTableList = finalPixelTableListBCRPROV,
                                          species = species)
provinceTable <- makesummarizedTableFromPixels(tabName = "summarizedTableFromPixelsProv.rds", 
                                               maskedDensityRasFolder = maskedDensityRasFolder, 
                                               column = "PROV", finalPixelTableList = finalPixelTableListBCRPROV,
                                               species = species)
provinceBcrTable <- makesummarizedTableFromPixels(tabName = "summarizedTableFromPixelsProv_BCR.rds", 
                                                  maskedDensityRasFolder = maskedDensityRasFolder, 
                                                  column = "PROV_BCR", finalPixelTableList = finalPixelTableListBCRPROV, 
                                                  species = species)

# RUN ON LOCAL COMPUTER ONLY, AFTER UPLOADING TABLES AND DOWNLOADING TO FOLDER DATA
borealTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixels.rds"))
bcrTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixelsBCR.rds"))
provinceTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixelsProv.rds"))
provinceBcrTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixelsProv_BCR.rds"))


        #########
        # PLOTS #
        #########

# HERE!!! <<<~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

reg <- summarizedTable

# ABOUT TO IMPLEMENT THIS -- BUT FIRST SUMMARIZE THINGS BY PROVINCE!
# lapply(unique(summarizedTable$region), function(reg){
#   
# })
# Prepping to compare our data to Rosenberg 2019
rosenberg2019 <- prepInputs(url = "https://drive.google.com/open?id=1nZDjJs4-NED0ic4uhL6ZM-M_i8Svr5aq",
                            destinationPath = dirname(maskedDensityRasFolder), targetFile = "popChangeRosenberg2019.csv",
                            fun = "data.table::fread")
names(rosenberg2019)[names(rosenberg2019) == "Four Letter Code"] <- "species"
rosenberg2019[, V1 := NULL]
summarizedTableCom <- merge(rosenberg2019, summarizedTable, by = "species")
# Check if the values we found are inside their CI95%
# 1. Create the values expected for 1985 and 2011 using Rosenberg 2019. For that, need to recreate the population in 1970
summarizedTableCom[, c("abund1970_R", "minAbund1970_R", "maxAbund1970_R") := list(popest+Loss_med, popestlci+Loss_lci, popestuci+Loss_uci)]
# 2. Calculate 1985 based on average yearly loss 
summarizedTableCom[, c("abund1985_R", "minAbund1985_R", "maxAbund1985_R") := list(abund1970_R-(abund1970_R*(1985-1970)*lossPercYear), 
                                                                                  minAbund1970_R-(minAbund1970_R*(1985-1970)*lossUpPercYear), 
                                                                                  maxAbund1970_R-(maxAbund1970_R*(1985-1970)*lossLoPercYear))]
# 3. Calculate 2011 based on average yearly loss 
summarizedTableCom[, c("abund2011_R", "minAbund2011_R", "maxAbund2011_R") := list(abund1970_R-(abund1970_R*(2011-1970)*lossPercYear), 
                                                                                  minAbund1970_R-(minAbund1970_R*(2011-1970)*lossUpPercYear), 
                                                                                  maxAbund1970_R-(maxAbund1970_R*(2011-1970)*lossLoPercYear))]


# Not sure here... Can I or can't I have this? Does it really not work if focal is not cummulative? I believe I was wrong. I think it does work! [ 12th SEPT ]
# PLOT1: rate of decrease for each year -- is forestry reducing? (DOES PIXEL vs TOTAL AGREE? -- only works if focal is not cummulative, # so it is not working, cause focal is cummulative):. Therefore we can only do it regarding the totals [ August ]
 
# HOW DO I KNOW the general effects of forestry are reducing in birds? If the 'rate' from one year is smaller than the previous
source(file.path(getwd(), "functions/effectsOfForestryWithTime.R"))
plot1 <- effectsOfForestryWithTime(fullTableList = fullTableAllBirds, pathToSave = dirname(maskedDensityRasFolder), 
                                   calculate = TRUE, addError = TRUE, separatePlots = TRUE) # TODO add to this plot the one showing forestry activities!

# PLOT2: How much did habitat supply change from 1984 to the given year (cummulative rate of habitat loss) 
# Exclusively related to forest activities
source(file.path(getwd(), "functions/changeInHabitatSupplyThroughTime.R"))
plot2 <- changeInHabitatSupplyThroughTime(fullTableList = fullTableAllBirds, pathToSave = dirname(maskedDensityRasFolder), 
                                          calculate = TRUE, addError = FALSE, separatePlots = FALSE)
# WHICH SPECIES DID WE LOOSE MOST? ARE THESE THE MOST ABUNDANT ONES?  OR ARE THESE MORE RELATED TO MATURE FOREST?

# PLOT4: For each species, the percentage of pixels that showed significant decline: ~~ NOT FOR NOW...

# significant <- sum(significantSlope[significantSlope > 0])
# allNotNA <- sum(significantSlope[!is.na(significantSlope)])
# allNA <- sum(significantSlope[is.na(significantSlope)])
# nonSignificant <- sum(significantSlope[significantSlope == 0])
# if (!all(significant + nonSignificant == allNotNA, 
#      significant + nonSignificant + allNA == ncell(significantSlope),
#      allNotNA + allNA == ncell(significantSlope)))
#   stop("Something is not adding up. Debug")

# MAPS

# MAP 1: Map of change in disturbance: mergedFocal2011 -- only works if this is cummulative (which is)! [ UPLOADED ]
    mergedFocal2011 <- raster("/mnt/data/Micheletti/borealBirdsAndForestry/modules/focalCalculation/data/mergedFocal2011-500Res250m.tif")
    
    
# MAP2: Change in habitat supply (absolute number -- realAbund2011-realAbund0) for each species
    d <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif"))
    source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/changeInHabitatSupply.R')
      absoluteHabitatSupplyMAP <- changeInHabitatSupply(tble = pixelTablesWithUncertaintyPre05,
                                                        RTM = d,
                                                        whichType = "absolute", # absolute, proportional
                                                        whichBirds = species, # "BIRD" OR "all"
                                                        pathToSave = dirname(maskedDensityRasFolder),
                                                        upload = FALSE) # type = each one of the columns for the maps below... The map needs to be uploaded to GDrive
      # NOT JUST MAP, BUT ALSO PLOT OF SUMMARY -- FOR EACH BIRD
      
# MAP2B: Change in habitat supply (absolute number -- realAbund2011-realAbund0) for all species together
      d <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif"))
      source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/changeInHabitatSupplyAll.R')
      absoluteHabitatSupplyMAPall <-  changeInHabitatSupplyAll(tble = pixelTablesWithUncertaintyPre05,
                                                            RTM = d,
                                                            whichType = "absolute", # absolute, proportional
                                                            pathToSave = dirname(maskedDensityRasFolder)) # type = each one of the columns for the maps below... The map needs to be uploaded to GDrive
      
      # NOT JUST MAP, BUT ALSO PLOT OF SUMMARY --  IN TOTAL
      # WHERE DID WE LOSE MORE SPECIES?
      
# Plot 6
      # WHAT IS THE PERCENT OF AREA AFFECTED BY ANY CHANGE IN BIRD ABUNDANCE FOR EACH SPECIES? --> SAVED THIS OBJ!
      fl <- usefun::grepMulti(x = list.files(dirname(maskedDensityRasFolder), #file.path(getwd(), "modules/focalCalculation/data/", "outputs/posthocAnalysis/"
                                             full.names=T), patterns = "percentChange")
    allChanges <- rbindlist(lapply(X = fl, FUN = function(ch){
      change <- readRDS(ch)
      species <- usefun::substrBoth(tools::file_path_sans_ext(ch), howManyCharacters = 4, fromEnd = TRUE)
      dt <- data.table(species = species, change = change)
    }))
    addTo <- data.table(species = c("BBWA", "BLPW"), change = c(0.24, 0.03))
    allChanges <- rbind(allChanges, addTo)
    # If I don't have time: BBWA = 24%; BLPW = 3%
    setkey(allChanges, "change")
    allChanges$species <- factor(allChanges$species, levels = allChanges$species)
    library("ggplot2")
    p <- ggplot(data = allChanges, aes(x = species, y = change, fill = species)) +
      geom_bar(stat = "identity") +
      theme(legend.position = "none")

# MAP3: Change in habitat supply (proportional -- (minrealAbund2011-realAbund0)/realAbund0) AND (maxrealAbund2011-realAbund0)/realAbund0) -- 
    # NOT JUST MAP, BUT ALSO PLOT OF SUMMARY
    # for each species (Especially SAR?)
    # HOW MUCH THE LOSS IS COMPARED TO POPULATION ABUNDANCE PROJECTIONS (abund0)?
    d <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif"))
    source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/changeInHabitatSupply.R')
        proportionalHabitatSupplyMAPeach <- Cache(changeInHabitatSupply, tble = pixelTablesWithUncertaintyPre05,
                                                          whichType = "proportional", # absolute, proportional
                                                          RTM = d,
                                                          pathToSave = dirname(maskedDensityRasFolder)) # type = each one of the columns for the maps below... The map needs to be uploaded to GDrive

      # MAP4: Change in habitat supply (proportional -- (minrealAbund2011-realAbund0)/realAbund0) AND (maxrealAbund2011-realAbund0)/realAbund0) -- 
      # NOT JUST MAP, BUT ALSO PLOT OF SUMMARY
      # for each species (Especially SAR?)
      # HOW MUCH THE LOSS IS COMPARED TO POPULATION ABUNDANCE PROJECTIONS (abund0)?
        d <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif"))
        source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/changeInHabitatSupply.R')
      proportionalHabitatSupplyMAPall <-  Cache(changeInHabitatSupplyAll, tble = pixelTablesWithUncertaintyPre05, # LATER
                                                            whichType = "proportional", # absolute, proportional
                                                            RTM = d,
                                                            pathToSave = dirname(maskedDensityRasFolder)) # type = each one of the columns for the maps below... The map needs to be uploaded to GDrive
    
      
# PLOT3: For each species, check if the decrease is happening in areas where the birds 
    # are already not too abundant: Expected density for each combination of 
    # BCR_Prov_LCC (densityBIRD.tif/realAbund0) vs MAP3 (Proportional Change):
    # correlation test plot for each sp, and then the average proportionalChange X realAbund0 
    # with all species?
    #  Same as plot5 but with my own data! 
      library("ggplot2")
      colsToKeep1 <- c("species", 
                       "propDiffExp", "propDiffMin1985", "propDiffMax1985")
      colsToKeep2 <- c("species", 
                       "abund1985", "minAbund1985", "maxAbund1985")
      colNames <- c("species", "expected", "minimum", "maximum")
      dt1 <- summarizedTableCom[, ..colsToKeep1]
      dt0 <- c(dt1$propDiffExp, dt1$propDiffMin1985, dt1$propDiffMax1985)
      dt0 <- data.table(species = dt1$species, proportionalDifference = dt0, type = rep(c("expected", "minimum", "maximum"), each = 15))
      
      dt2 <- summarizedTableCom[, ..colsToKeep2]
      dt3 <- c(dt2$abund1985, dt2$minAbund1985, dt2$maxAbund1985)
      dt3 <- data.table(species = dt2$species, abundance2011_Solymos2013 = dt3, type = rep(c("expected", "minimum", "maximum"), each = 15))
      
      dt <- merge(dt0, dt3)
      
      typeForPlot <- "expected"
      p <- ggplot(data = dt[type == typeForPlot], aes(x = proportionalDifference,
                                                      y = abundance2011_Solymos2013, color = species, label = species)) +
        geom_point(size = 4) +
        geom_text(aes(label = species, color = species), hjust = 0.5, vjust = -1) + 
        labs(x = "Proportional Difference from 2011 in relation to 1985 ((2011-1985)/1985)",
             y = "Abundance of birds (Solymos 2013)") +
        theme(legend.position =  "none") # GROUP THE BIRDS IN SOME CATEGORY? Not that I could see...
      
# PLOT4: Compare our calculations to Rosenberg 2019

      colsToKeep1 <- c("species", 
                      "abund1985", "minAbund1985", "maxAbund1985")
      colsToKeep2 <- c("species", 
                       "abund2011")
      colsToKeep3 <- c("species", 
                       "abund1985_R", "minAbund1985_R", "maxAbund1985_R")
      colsToKeep4 <- c("species", 
                       "abund2011_R", "minAbund2011_R", "maxAbund2011_R")
      colNames <- c("species", "abund", "minAbund", "maxAbund", "year")
      dt1 <- summarizedTableCom[, ..colsToKeep1]
      dt1[, "year" := 1985]
      names(dt1) <- colNames

      dt2 <- summarizedTableCom[, ..colsToKeep2]
      dt2[, c("minAbund2011", "maxAbund2011") := abund2011]
      dt2[, "year" := 2011]
      names(dt2) <- colNames

      dt1 <- rbind(dt1, dt2) 
      dt1$dataset <- "presentWork"
      
      dt3 <- summarizedTableCom[, ..colsToKeep3]
      dt3[, "year" := 1985]
      names(dt3) <- colNames
      
      dt4 <- summarizedTableCom[, ..colsToKeep4]
      dt4[, "year" := 2011]
      names(dt4) <- colNames
      
      dt2 <- rbind(dt3, dt4) 
      dt2$dataset <- "Rosenber2019"
      dt <- rbind(dt1, dt2, use.names = FALSE)
      dt <- rbind(dt1, dt2) 
      
      library("ggplot2")
      scipOrig <- getOption("scipen")
      options(scipen=10000)
      on.exit(options(scipen = scipOrig))
      p <- ggplot(data = dt, aes(x = species,
                                     y = abund, fill = dataset)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        geom_errorbar(aes(ymin = minAbund, ymax = maxAbund), position = position_dodge()) +
        facet_grid(year ~ .) + 
      labs(y = "Abundance of birds") +
        theme(legend.title = element_blank(),
              legend.position = "bottom")
      
# PLOT5: Which species presented the highest percentage loss in comparison to its population size? are these the most abundant? There is such trend
      # Should I calculate the proportional difference also for Rosenberg 2019?
      
      colsToKeep1 <- c("species", 
                       "propDiffExp", "propDiffMin1985", "propDiffMax1985")
      colsToKeep2 <- c("species", 
                       "abund1985_R", "minAbund1985_R", "maxAbund1985_R")
      colNames <- c("species", "expected", "minimum", "maximum")
      dt1 <- summarizedTableCom[, ..colsToKeep1]
      dt0 <- c(dt1$propDiffExp, dt1$propDiffMin1985, dt1$propDiffMax1985)
      dt0 <- data.table(species = dt1$species, proportionalDifference = dt0, type = rep(c("expected", "minimum", "maximum"), each = 15))

      dt2 <- summarizedTableCom[, ..colsToKeep2]
      dt3 <- c(dt2$abund1985_R, dt2$minAbund1985_R, dt2$maxAbund1985_R)
      dt3 <- data.table(species = dt2$species, abundance2011_Rosenberg2019 = dt3, type = rep(c("expected", "minimum", "maximum"), each = 15))
      
      dt <- merge(dt0, dt3)
      
      typeForPlot <- "expected"
      p <- ggplot(data = dt[type == typeForPlot], aes(x = proportionalDifference,
                                 y = abundance2011_Rosenberg2019, color = species, label = species)) +
        geom_point(size = 4) +
        geom_text(aes(label = species, color = species), hjust = 0.5, vjust = -1) + 
        labs(x = "Proportional Difference from 2011 in relation to 1985 ((2011-1985)/1985)",
             y = "Abundance of birds") +
        theme(legend.position =  "none") # GROUP THE BIRDS IN SOME CATEGORY? Not that I could see...
      
      
      
      
