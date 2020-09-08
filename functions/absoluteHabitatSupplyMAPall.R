options(reproducible.useCache = TRUE)
library("reproducible")
library("raster")
library("data.table")
library("usefun")
library("magrittr")
wd <- getwd()
if (all(pemisc::user() %in% c("Tati", "tmichele"), wd != "/home/tmichele/projects/borealBirdsAndForestry")){
  warning("Make sure you are in the correct working directory!")
  # setwd("/home/tmichele/projects/borealBirdsAndForestry")
}
maskedDensityRasFolder <- file.path(wd, "outputs/posthocAnalysis/maskedDensityRas")
originalDensFolder <- file.path(wd, "modules/birdDensityBCR_Prov_LCC/data")
source(file.path(getwd(), "functions/returnBirdAbundance.R"))


# googledrive::drive_auth(use_oob = TRUE) # ONLY ONCE: USING RStudio Server. Doesn't work for the first time in RGui
googledrive::drive_auth(email = "tati.micheletti@gmail.com")
# googledrive::drive_deauth()
SpaDES.core::setPaths(cachePath = file.path(getwd(), "cache"))
species <- c("BBWA", "BLPW", "BOCH", "BRCR", "BTNW", "CAWA", "CMWA", "CONW", "OVEN", "PISI", "RBNU", "SWTH", "TEWA", "WETA", "YRWA")
onlyFinalPixelTables <- TRUE # If I only want the final full pixel tables (not the summarized ones!), 
# this should be TRUE
doAssertions <- TRUE
freeUpMem <- FALSE
lightLoadFinalTable <- TRUE

pixelTablesWithUncertaintyPre05 <- lapply(X = species, FUN = function(BIRD){
  finalFolder <-  reproducible::checkPath(file.path(getwd(), "outputs/posthocAnalysis/finalFullTables"), create = TRUE)
  finalFullTablePath <- file.path(finalFolder, paste0("finalFullTable", BIRD, ".rds"))
  if (!file.exists(finalFullTablePath)){
    message(crayon::red(paste0("finalFullTable for ", BIRD, " does not exist. Creating...")))
    densityTable <- Cache(prepInputs, url = "https://drive.google.com/open?id=1SEcJdS25YkIoRMmrgGNe4-HKG90OtYjX",
                          targetFile = "Habitat_Association_by_jurisdiction(bamddb_Apr19-2012).csv", # OBS. Not all species have densities for all combinations of LCC_PROV_BCR. Not all species are everywhere!
                          destinationPath = asPath(pathData), fun = "data.table::fread",
                          userTags = c("objectName:densityEstimates", "script:paperPlots")) # Also checked if it was pooled with other BCR, but not.
    densityTable <- densityTable[SPECIES %in% species,]
    source(file.path(getwd(), 'functions/createBCR_PROV_LCC_EstimatesPosthoc.R'))
    BCR_Prov_LCC <- Cache(createBCR_PROV_LCC_EstimatesPosthoc, BCR = BCRLCC05$BCR,
                          LCC05 = BCRLCC05$LCC05, justBCRProvLCC = TRUE, pathToDSave = maskedDensityRasFolder, 
                          densityMap = file.path(maskedDensityRasFolder, "densityBBWA.tif"), # Can be used for all species as it is a template for rtm! 
                          omitArgs = c("userTags", "useCache"),
                          userTags = c("objectName:BCR_Prov_LCC", "script:paperPlot"))
    
    # Subset BCR_Prov_LCC to the pixels where we actually have any data:
    BIRDfullTable <- readRDS(fullTableAllBirds[[BIRD]])
    setkey(BIRDfullTable, "pixelID")
    pixelsWithData <- BIRDfullTable[["pixelID"]]
    
    # Exclude uninportant columns 
    densityTableSub <- densityTable[SPECIES == BIRD, c("LCC", "BCR", "PROV", "D", "D_se")]
    # Merge to get the Density per BCR_Prov_LCC per pixel
    densityTableSubPixelID <- Cache(merge, BCR_Prov_LCC, densityTableSub, by = c("BCR", "PROV", "LCC"), 
                                    all.x = TRUE, omitArgs = c("useCache"), 
                                    userTags = c("script:paperPlots", "objectName:densityTableSubPixelID"))
    setkey(densityTableSubPixelID, "pixelID")
    
    # assertion regarding the original density form Solymos&Stralber being the same as realAbund0/mostLikelyAbund and abundance from the full tables
    # browser() # Check the table now ==> Assertions are fine... go figure?!
    if (doAssertions){ # For BLPW (and maybe other birds?) a few pixels are not agreeing (Abund and realAbund0|originalDMaps). 
      # Anywhere where I use Abund, I should be using realAbund0 instead until I figure this one out 
      # (Should NOT turn off this assertion because I fixed it up in here. Exchanged Abund for realAbund0. If the assertion fails, I'm in shit...)! [19Sept19]
      # 1. originalDMaps <- DENSITY MAPS * 6.25
      originalDens <- raster(file.path(wd, paste0("outputs/posthocAnalysis/maskedDensityRas/density", BIRD, ".tif")))
      originalDensPostProc <- Cache(reproducible::postProcess, originalDens, rasterToMatch = NULL, #BCRLCC05$LCC05, 
                                    userTags = c("script:paperPlots", "goal:snappingDensityRas", paste0("species:", BIRD)))
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
      if (!ok) message(crayon::red("The density from one of the sources (probably D/Abund, coming from the created LCC_PROV_BCR) does not match the others. 
                                   A fix will be applied but this should be debugged ASAP."))
      # Here is my plan: realAbund0 is working fine (in accordance with the original density maps). So here I replace
      # D with realAbund0. This way I can potentially fix this problem. Should be revised later, though [19Sep19]
      # TODO
      realAbund0 <- BIRDfullTable[, c("pixelID", "realAbund0")]
      densityTableSubPixelID <- merge(densityTableSubPixelID, realAbund0, by = "pixelID", all.x = TRUE)
      # Does the number of pixels with values match D and realAbund0? If so, it should be safe to replace one with the other
      ok <- length(!is.na(densityTableSubPixelID$D)) == length(!is.na(densityTableSubPixelID$realAbund0))
      if (!ok) stop("For some wicked reason, the fix applied did not work. NA's in D don't match realAbund0. 
                    Good luck fixing it! I'm out!")
      message(crayon::green("Ufff! The fix worked! Fine for now, but please debug it ASAP."))
      densityTableSubPixelID[, D := NULL]
      names(densityTableSubPixelID)[names(densityTableSubPixelID) == "realAbund0"] <- "D"
      # Divide the new D for 6.25 as it comes from the realAbund0, which is already in abundance form (density*6.25).
      densityTableSubPixelID$D <- densityTableSubPixelID$D/6.25
    }
    
    # The densities (D) of pixels that have no 'se' (i.e. D_se == NA) are likely just artifacts. We should set these to 0 (i.e. the birds are technically not there. 
    # Converting to NA would be a possibility, but I think 0 is better as we know it is being predicted there (no holes on the map) 
    # 1. Checked the range of values that have NA in D_se but something in D so I can make a cut in a bin that makes sense. Here we found out that we have a few values between 0.005 and 0.0052. 
    # These might not be artifacts so I will keep these even though their D_se is NA. I will cut them off in 0.00000001
    # DwhereSEisNA <- densityTableSubPixelID[is.na(D_se), D]
    # br <- seq(0, 0.006, by = 0.001)
    # ranges <- paste(head(br, -1), br[-1], sep = " - ")
    # freq <- hist(DwhereSEisNA, breaks = br, include.lowest=TRUE, plot = TRUE)
    # data.frame(range = ranges, frequency = freq$counts)
    # onlyLower <- DwhereSEisNA[DwhereSEisNA < 0.002 & !is.na(DwhereSEisNA)]
    # max(onlyLower) # 1.28e-08
    densityTableSubPixelID[D < 0.0000001 & is.na(D_se), D := 0]
    
    # Get the min, max and average D for each combination of BCR and PROV, among all forested LCC classes (1:15)
    # maybe need to include LCC classes 20 and 25
    densityTableSubPixelID[, validD := ifelse(!is.na(D) & D > 0, D, NA)] # As zero's should not be considered for the interval, as they are derived from artifacts!
    densityTableSubPixelID[LCC %in% c(1:15), c("minD", "maxD") := list(min(validD, na.rm = TRUE), 
                                                                       max(validD, na.rm = TRUE)), by = c("BCR", "PROV")] # When we have NA's in minD and maxD means we have LCC that is not forest.
    # Here we fix the min and max for non-forests (we set both to D, so no variation), remembering this should ONLY happen for YYYY =< 2005
    densityTableSubPixelID[is.na(minD) | is.na(maxD), c("minD", "maxD") := validD] 
    # If Abund is NA, min and max need to be NA as well (i.e. abund is NA for pixels without prediction, so it can't have min or max!)
    densityTableSubPixelID[is.na(validD), c("minD", "maxD") := NA] 
    
    if (freeUpMem){
      rm(BCR_Prov_LCC, envir = .GlobalEnv)
      rm(densityTable, envir = .GlobalEnv)
      gc()
    }
    
    # simplify tables so I don't end up with monsters
    densityTableSubPixelIDsimp <- densityTableSubPixelID[, c("pixelID", "D_se", "D", "minD", "maxD")]
    rm(densityTableSubPixelID); gc()
    # 1. Multiply density by 6.25 to convert to Abundance (D, D_se, minD, maxD)
    # Could exclude aveD... doesn't make ecological sense! If anything, the closest to be correct one is is 'realAbund'
    # 2. Keep D and D_se so we can have the "most likely scenario" and uncertainty!
    if (doAssertions){
      ok <- all(names(densityTableSubPixelIDsimp) == c("pixelID", "D_se", "D", "minD", "maxD"))
      if (!ok) stop("Names of densityTableSubPixelIDsimp are not matching the order, please debug!")
    }
    DtoAbund <- c("D_se", "D", "minD", "maxD")
    densityTableSubPixelIDsimp[, (DtoAbund) := lapply(.SD, function(x)
      x * 6.25), .SDcols = DtoAbund]
    names(densityTableSubPixelIDsimp) <- c("pixelID", "Abund_se", "Abund", "minAbund", "maxAbund")
    
    # 3. Identify which pixels changed before 2005 (just subtract cummRate2005 from cummRate1985 and anything other than 0) and simplify table
    # assertion regarding the data coming from the species in question
    if (doAssertions){
      if (BIRD != unique(BIRDfullTable[,species])) 
        stop("Bird species specified and species in the bird table are not the same. Debug.")
    }
    
    # simplify table
    colsToDrop <- names(BIRDfullTable)[!names(BIRDfullTable) %in% c("pixelID", paste0("realAbund", c(0, 1984:2011)),
                                                                    grepMulti(x = names(BIRDfullTable), 
                                                                              patterns = "cum"))]
    BIRDfullTable[, (colsToDrop) := NULL]
    # 4. Create a new table: with realAbundYEARmin realAbundYEARmax, where for pixels that didn't change, 
    # these two columns have the same value (the original realAbundYEAR)
    # 4.1 Join the table with Abund, Abund_se, minAbund, maxAbund
    BIRDfullTable <- merge(BIRDfullTable,
                           densityTableSubPixelIDsimp, by = "pixelID", all.x = TRUE)
    
    # Identify which pixels changed before 2005
    BIRDfullTable[, pixelChanged := ifelse(cummRate1985-cummRate2005 != 0, TRUE, FALSE)] #all(LCC %in% 1:15)
    source(file.path(getwd(), '/functions/percentOfAreaAffectedByDisturbance.R'))
    percChange <- percentOfAreaAffectedByDisturbance(tableWithChange = BIRDfullTable, logicalColChange = "pixelChanged")
    message(crayon::cyan(paste0("Percent area that is affected by ANY change for ", BIRD, ": ", 
                                100*round(as.numeric(percChange), 2), "%")))
    saveRDS(percChange, file.path(dirname(maskedDensityRasFolder), 
                                  paste0("percentChange", BIRD, ".rds")))
    
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
        BIRDfullTable[, c(paste0("min", newColName), paste0("max", newColName)) := realAbund0 + (get(cum)*realAbund0)]
      }
    })
    # 4.3 Simplify and save new table...
    saveRDS(BIRDfullTable, finalFullTablePath)
  } else {
    if (lightLoadFinalTable){
      message(crayon::green(paste0("finalFullTable (predictions + rates + ",
                                   "real abundances per year)  for "
                                   , BIRD, " exists. Returning path...")))
      return(finalFullTablePath)
    } else {
      message(crayon::green(paste0("finalFullTable (predictions + rates + ",
                                   "real abundances per year)  for "
                                   , BIRD, " exists. Returning table...")))
      return(readRDS(finalFullTablePath))
    }
  }
})
names(pixelTablesWithUncertaintyPre05) <- species

d <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif"))
source(file.path(getwd(), 'functions/changeInHabitatSupplyAll.R')
absoluteHabitatSupplyMAPall <-  changeInHabitatSupplyAll(tble = pixelTablesWithUncertaintyPre05,
                                                      RTM = d,
                                                      whichType = "absolute", # absolute, proportional
                                                      pathToSave = dirname(maskedDensityRasFolder)) # type = each one of the columns for the maps below... The map needs to be uploaded to GDrive
