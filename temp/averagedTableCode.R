# THIS IS TO CREATE THE AVERAGED TABLES, NOT PIXEL BASED. ITS PROBABLY REALLY WRONG. Should potentially not be ran.
# You are warned. XD

if (FALSE){
  completeSummaryFile <- file.path(wd, "outputs/posthocAnalysis", "completeSummaryTable.rds")
  if (file.exists(completeSummaryFile)){
    fullTableList <- readRDS(completeSummaryFile) # JUST FOR NOW!
  } else {
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

