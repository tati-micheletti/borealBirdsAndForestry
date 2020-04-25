
# MAKE PLOTS!

# [Note 12APRIL20: I should only add BCR, PROV, BCR_PROV and Alberta's 'value' column to existing 
# table and have only one big final table! This way I can use the same table for all maps... 
# just need to rename columns]
# finalPixelTableList
# finalPixelTableListBCRPROV
# finalPixelTableListAlberta
# [13APRIL20: Not as simple as it seems... Will leave for later]

# CREATING FINAL TABLES
# [13APR20: These tables are general tables summarized for regions with:
# c("species", "region", "abund0", "abund1985", "minAbund1985", 
# "maxAbund1985", "abund2011", "diff2011_1985min", "diff2011_1985max", 
# "diff2011_1985exp", "diff2011_0", "range", "diffPerYear0", "diffPerYearMin", 
# "diffPerYearMax", "diffPerYearExp", "propDiff0", "propDiffExp", 
# "propDiffMin1985", "propDiffMax1985"). Therefore, these are not the ones to make 
# through time plots]
# source(file.path(getwd(), 'functions/makesummarizedTableFromPixels.R'))
# borealTable <- makesummarizedTableFromPixels(tabName = paste0("summarizedTableFromPixelsBoreal", 
#                                                               spatialScale,"m.rds"), 
#                                              foldWhereToSave = folderForTables, 
#                                              column = "value", 
#                                              finalPixelTableList = finalPixelTableList,
#                                              species = species)
# bcrTable <- makesummarizedTableFromPixels(tabName = paste0("summarizedTableFromPixelsBCR", 
#                                                            spatialScale,"m.rds"), 
#                                           foldWhereToSave = folderForTables, 
#                                           column = "BCR",  
#                                           finalPixelTableList = finalPixelTableListBCRPROV,
#                                           species = species)
# provinceTable <- makesummarizedTableFromPixels(tabName = paste0("summarizedTableFromPixelsProv", 
#                                                                 spatialScale,"m.rds"), 
#                                                foldWhereToSave = folderForTables, 
#                                                column = "PROV", 
#                                                finalPixelTableList = finalPixelTableListBCRPROV,
#                                                species = species)
# provinceBcrTable <- makesummarizedTableFromPixels(tabName = paste0("summarizedTableFromPixelsBCR_Prov", 
#                                                                    spatialScale,"m.rds"), 
#                                                   foldWhereToSave = folderForTables, 
#                                                   column = "PROV_BCR", 
#                                                   finalPixelTableList = finalPixelTableListBCRPROV, 
#                                                   species = species)
# 
# albertaTable <- makesummarizedTableFromPixels(tabName = paste0("summarizedTableFromPixelsAlberta", 
#                                                                    spatialScale,"m.rds"), 
#                                                   foldWhereToSave = folderForTables, 
#                                                   column = "value", 
#                                                   finalPixelTableList = finalPixelTableListAlberta, 
#                                                   species = species)
# 
# 
# quebecTable <- makesummarizedTableFromPixels(tabName = paste0("summarizedTableFromPixelsQuebec",
#                                                                    spatialScale,"m.rds"),
#                                                   foldWhereToSave = folderForTables,
#                                                   column = "value",
#                                                   finalPixelTableList = finalPixelTableListQuebec,
#                                                   species = species)

# RUN ON LOCAL COMPUTER ONLY, AFTER UPLOADING TABLES AND DOWNLOADING TO FOLDER DATA
# borealTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixelsBoreal500m.rds"))
# bcrTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixelsBCR500m.rds"))
# provinceTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixelsProv500m.rds"))
# provinceBcrTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixelsProv_BCR500m.rds"))
# quebecTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixelsQuebec500m.rds"))
# albertaTable <- readRDS(file.path(getwd(), "data/summarizedTableFromPixelsAlberta500m.rds"))


        #########
        # PLOTS #
        #########
library("reproducible")

# Prepping to compare our data to Rosenberg 2019
rosenberg2019 <- prepInputs(url = "https://drive.google.com/open?id=1nZDjJs4-NED0ic4uhL6ZM-M_i8Svr5aq",
                            destinationPath = ifelse(pemisc::user() == "tmichele", 
                                                     posthocFolder,
                                                     file.path(getwd(), "data")), 
                            targetFile = "popChangeRosenberg2019.csv",
                            fun = "data.table::fread")
names(rosenberg2019)[names(rosenberg2019) == "Four Letter Code"] <- "species"
rosenberg2019[, V1 := NULL]
summarizedTableCom <- merge(rosenberg2019, borealTable[region == 1,], by = "species") # NOT SURE THIS IS RIGHT... Think 'boreal' is wrong
# Check if the values we found are inside their CI95%
# 1. Create the values expected for 1985 and 2011 using Rosenberg 2019. 
# For that, need to recreate the population in 1970
summarizedTableCom[, c("abund1970_R", "minAbund1970_R", "maxAbund1970_R") := 
                     list(popest+Loss_med, popestlci+Loss_lci, popestuci+Loss_uci)]
# 2. Calculate 1985 based on average yearly loss 
summarizedTableCom[, c("abund1985_R", "minAbund1985_R", "maxAbund1985_R") := 
                     list(abund1970_R-(abund1970_R*(1985-1970)*lossPercYear),
                          minAbund1970_R-(minAbund1970_R*(1985-1970)*lossUpPercYear),
                          maxAbund1970_R-(maxAbund1970_R*(1985-1970)*lossLoPercYear))]
# 3. Calculate 2011 based on average yearly loss 
summarizedTableCom[, c("abund2011_R", "minAbund2011_R", "maxAbund2011_R") := 
                     list(abund1970_R-(abund1970_R*(2011-1970)*lossPercYear),
                          minAbund1970_R-(minAbund1970_R*(2011-1970)*lossUpPercYear),
                          maxAbund1970_R-(maxAbund1970_R*(2011-1970)*lossLoPercYear))]
saveRDS(summarizedTableCom, file.path(ifelse(pemisc::user() == "tmichele", 
                                              folderForTables,
                                              file.path(getwd(), "data")), 
                                       paste0("comparisonTableRosenberg", 
                                              spatialScale,"m.rds")))

# Not sure here... Can I or can't I have this? Does it really not work if focal is
#  not cummulative? I believe I was wrong. I think it does work! [ 12th SEPT ]
# PLOT1: rate of decrease for each year -- is forestry reducing? (DOES PIXEL vs TOTAL AGREE? 
# -- only works if focal is not cummulative, # so it is not working, cause focal is cummulative):. 
# Therefore we can only do it regarding the totals [ August ]
 

# PLOT2: How much did habitat supply change from 1984 to the given year 
# (cummulative rate of habitat loss) 
# Exclusively related to forest activities
  #### THIS IS POTENTIALLY WRONG, AS ITS NOT PIXEL BASED!
  source(file.path(getwd(), "functions/effectsOfForestryWithTime.R"))
  namesTabs <- c("Managed Forest", "Unmanaged Forest") # Names you want
  names(namesTabs) <- c("1", "2") # Names in the dataset
  plot2 <- effectsOfForestryWithTime(fullTableList = finalPixelTableList,
                                     spatialScale = spatialScale,
                                     tableName = "ManagedUnmanaged",
                                     pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                         folderForTables,
                                                         file.path(getwd(), "data")),
                                     subsetName = "value", birdSpecies = NULL,
                                     patternToPlot = "cummRate", whichPlot = "p2",
                                     calculate = TRUE, namesTabs = namesTabs,
                                     whichOp = "mean")
  
  plot2b <- effectsOfForestryWithTime(fullTableList = finalPixelTableList,
                                      spatialScale = spatialScale,
                                      tableName = "ManagedUnmanaged",
                                      pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                          folderForTables,
                                                          file.path(getwd(), "data")),
                                      subsetName = "value", birdSpecies = NULL,
                                      patternToPlot = "cummRate", whichPlot = "p2",
                                      calculate = TRUE, namesTabs = namesTabs,
                                      whichOp = "min")

  namesTabs <- c("ALPAC") # Names you want
  names(namesTabs) <- c("1") # Names in the dataset
  plot2Alberta <- effectsOfForestryWithTime(fullTableList = finalPixelTableListAlberta,
                                            spatialScale = spatialScale,
                                            tableName = "Alberta",
                                            pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                                folderForTables,
                                                                file.path(getwd(), "data")),
                                            patternToPlot = "cummRate", whichPlot = "p2",
                                            subsetName = "value", birdSpecies = NULL,
                                            calculate = TRUE, namesTabs = namesTabs)
  
  
  plot2bAlberta <- effectsOfForestryWithTime(fullTableList = finalPixelTableListAlberta,
                                            spatialScale = spatialScale,
                                            tableName = "Alberta",
                                            pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                                folderForTables,
                                                                file.path(getwd(), "data")),
                                            patternToPlot = "cummRate", whichPlot = "p2",
                                            subsetName = "value", birdSpecies = NULL,
                                            calculate = TRUE, namesTabs = namesTabs,
                                            whichOp = "min")
  
  namesTabs <- c("QUEBEC") # Names you want
  names(namesTabs) <- c("1") # Names in the dataset
  plot2Quebec <- effectsOfForestryWithTime(fullTableList = finalPixelTableListQuebec,
                                           spatialScale = spatialScale,
                                           tableName = "Quebec",
                                           pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                               folderForTables,
                                                               file.path(getwd(), "data")),
                                           patternToPlot = "cummRate", whichPlot = "p2",
                                           subsetName = "value", birdSpecies = NULL,
                                           calculate = TRUE, namesTabs = namesTabs)
# MAPS


#########################################################################

# MAP2: Change in habitat supply (absolute number -- realAbund2011-realAbund0) for each species
    d <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif")) # RTM
    source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/changeInHabitatSupply.R')
      absoluteHabitatSupplyMAP <- changeInHabitatSupply(tble = pixelTablesWithUncertaintyPre05,
                                                        # This needs to be done in 58. 
                                                        # pixelTablesWithUncertaintyPre05 == 
                                                        RTM = d,
                                                        whichType = "absolute", # absolute, proportional
                                                        whichBirds = species, # "BIRD" OR "all"
                                                        pathToSave = dirname(maskedDensityRasFolder),
                                                        upload = FALSE) # type = each one of the 
      # columns for the maps below... The map needs to be uploaded to GDrive
      # NOT JUST MAP, BUT ALSO PLOT OF SUMMARY -- FOR EACH BIRD
      
# MAP2B: Change in habitat supply (absolute number -- realAbund2011-realAbund0) for all species together
      d <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif"))
      source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/changeInHabitatSupplyAll.R')
      absoluteHabitatSupplyMAPall <-  changeInHabitatSupplyAll(tble = pixelTablesWithUncertaintyPre05,
                                                            RTM = d,
                                                            whichType = "absolute", # absolute, proportional
                                                            pathToSave = dirname(maskedDensityRasFolder)) 
      # type = each one of the columns for the maps below... The map needs to be uploaded to GDrive
      
      # NOT JUST MAP, BUT ALSO PLOT OF SUMMARY --  IN TOTAL
      # WHERE DID WE LOSE MORE SPECIES?
      
# Plot 6
      # WHAT IS THE PERCENT OF AREA AFFECTED BY ANY CHANGE IN BIRD ABUNDANCE FOR EACH SPECIES? --> SAVED THIS OBJ!
      fl <- usefun::grepMulti(x = list.files(dirname(maskedDensityRasFolder), 
                                             #file.path(getwd(), "modules/focalCalculation/data/", 
                                             #"outputs/posthocAnalysis/"
                                             full.names = TRUE), patterns = "percentChange")
    allChanges <- rbindlist(lapply(X = fl, FUN = function(ch){
      change <- readRDS(ch)
      species <- usefun::substrBoth(tools::file_path_sans_ext(ch), howManyCharacters = 4, 
                                    fromEnd = TRUE)
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
        proportionalHabitatSupplyMAPeach <- Cache(changeInHabitatSupply, 
                                                  tble = pixelTablesWithUncertaintyPre05,
                                                  whichType = "proportional", # absolute, proportional
                                                  RTM = d,
                                                  pathToSave = dirname(maskedDensityRasFolder)) # type = each one of the columns for the maps below... The map needs to be uploaded to GDrive

      # MAP4: Change in habitat supply (proportional -- (minrealAbund2011-realAbund0)/realAbund0) AND (maxrealAbund2011-realAbund0)/realAbund0) -- 
      # NOT JUST MAP, BUT ALSO PLOT OF SUMMARY
      # for each species (Especially SAR?)
      # HOW MUCH THE LOSS IS COMPARED TO POPULATION ABUNDANCE PROJECTIONS (abund0)?
        d <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif"))
        source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/changeInHabitatSupply.R')
      proportionalHabitatSupplyMAPall <-  Cache(changeInHabitatSupplyAll, 
                                                tble = pixelTablesWithUncertaintyPre05, # LATER
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
      dt0 <- data.table(species = dt1$species, proportionalDifference = dt0, 
                        type = rep(c("expected", "minimum", "maximum"), each = 15))
      
      dt2 <- summarizedTableCom[, ..colsToKeep2]
      dt3 <- c(dt2$abund1985, dt2$minAbund1985, dt2$maxAbund1985)
      dt3 <- data.table(species = dt2$species, abundance2011_Solymos2013 = dt3, 
                        type = rep(c("expected", "minimum", "maximum"), each = 15))
      
      dt <- merge(dt0, dt3)
      
      typeForPlot <- "expected"
      p <- ggplot(data = dt[type == typeForPlot], aes(x = proportionalDifference,
                                                      y = abundance2011_Solymos2013, 
                                                      color = species, label = species)) +
        geom_point(size = 4) +
        geom_text(aes(label = species, color = species), hjust = 0.5, vjust = -1) + 
        labs(x = "Proportional Difference from 2011 in relation to 1985 ((2011-1985)/1985)",
             y = "Abundance of birds (Solymos 2013)") +
        theme(legend.position =  "none") 
      # GROUP THE BIRDS IN SOME CATEGORY? Not that I could see...
      
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
      
# PLOT5: Which species presented the highest percentage loss in comparison to its population size? 
# are these the most abundant? There is such trend
      # Should I calculate the proportional difference also for Rosenberg 2019?
      
      colsToKeep1 <- c("species", 
                       "propDiffExp", "propDiffMin1985", "propDiffMax1985")
      colsToKeep2 <- c("species", 
                       "abund1985_R", "minAbund1985_R", "maxAbund1985_R")
      colNames <- c("species", "expected", "minimum", "maximum")
      dt1 <- summarizedTableCom[, ..colsToKeep1]
      dt0 <- c(dt1$propDiffExp, dt1$propDiffMin1985, dt1$propDiffMax1985)
      dt0 <- data.table(species = dt1$species, proportionalDifference = dt0, 
                        type = rep(c("expected", "minimum", "maximum"), each = 15))

      dt2 <- summarizedTableCom[, ..colsToKeep2]
      dt3 <- c(dt2$abund1985_R, dt2$minAbund1985_R, dt2$maxAbund1985_R)
      dt3 <- data.table(species = dt2$species, abundance2011_Rosenberg2019 = dt3, 
                        type = rep(c("expected", "minimum", "maximum"), each = 15))
      
      dt <- merge(dt0, dt3)
      
      typeForPlot <- "expected"
      p <- ggplot(data = dt[type == typeForPlot], aes(x = proportionalDifference,
                                 y = abundance2011_Rosenberg2019, 
                                 color = species, label = species)) +
        geom_point(size = 4) +
        geom_text(aes(label = species, color = species), hjust = 0.5, vjust = -1) + 
        labs(x = "Proportional Difference from 2011 in relation to 1985 ((2011-1985)/1985)",
             y = "Abundance of birds") +
        theme(legend.position =  "none") 
      # GROUP THE BIRDS IN SOME CATEGORY? Not that I could see...
      
      
      
      
