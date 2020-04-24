#########
# PLOTS #
#########

library("reproducible")
library("data.table")
species <- c("BBWA", "BLPW", "BOCH", "BRCR", "BTNW",
             "CAWA", "CMWA", "CONW", "OVEN", "PISI",
             "RBNU", "SWTH", "TEWA", "WETA", "YRWA")
spatialScale <- 500 # 500m DONE! 
posthocFolder <- file.path(getwd(), "outputs/posthocAnalysis")


#####################################################################################

# 1. How do I know if the general effects of forestry on birds are reducing through 
# time? If the 'rate' of change from one year is smaller than the previous: 
# (birdAbundanceInYear1-birdAbundanceInYear0)/birdAbundanceInYear0

#####################################################################################

scipOrig <- getOption("scipen")
options(scipen = 10000)
on.exit(options(scipen = scipOrig))

source(file.path(getwd(), "functions/effectsOfForestryWithTime.R"))
namesTabs <- c("Managed Forest", "Unmanaged Forest") # Names you want
names(namesTabs) <- c("1", "2") # Names in the dataset
plot1 <- effectsOfForestryWithTime(fullTableList = finalPixelTableList,
                                   spatialScale = spatialScale,
                                   tableName = "ManagedUnmanaged",
                                   pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                       folderForTables,
                                                       file.path(getwd(), "data")),
                                   subsetName = "value", birdSpecies = NULL,
                                   calculate = TRUE, namesTabs = namesTabs)

namesTabs <- c("ALPAC") # Names you want
names(namesTabs) <- c("1") # Names in the dataset
plot1Alberta <- effectsOfForestryWithTime(fullTableList = finalPixelTableListAlberta,
                                          spatialScale = spatialScale,
                                          tableName = "Alberta",
                                          pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                              folderForTables,
                                                              file.path(getwd(), "data")),
                                          subsetName = "value", birdSpecies = NULL,
                                          calculate = TRUE, namesTabs = namesTabs)

namesTabs <- c("QUEBEC") # Names you want
names(namesTabs) <- c("1") # Names in the dataset
plot1Quebec <- effectsOfForestryWithTime(fullTableList = finalPixelTableListQuebec,
                                         spatialScale = spatialScale,
                                         tableName = "Quebec",
                                         pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                             folderForTables,
                                                             file.path(getwd(), "data")),
                                         subsetName = "value", birdSpecies = NULL,
                                         calculate = TRUE, namesTabs = namesTabs)

# BCR
namesTabs <- c(paste0("BCR", unique(BCRdt$BCR))) # Names you want
names(namesTabs) <- unique(BCRdt$BCR) # Names in the dataset
plot1BCR <- effectsOfForestryWithTime(fullTableList = finalPixelTableListBCRPROV,
                                      spatialScale = spatialScale,
                                      tableName = "BCR",
                                      pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                          folderForTables,
                                                          file.path(getwd(), "data")),
                                      subsetName = "BCR", birdSpecies = NULL,
                                      calculate = TRUE, namesTabs = namesTabs)
# PROV
namesTabs <- unique(BCRdt$PROVINCE_S) # Names you want
names(namesTabs) <- unique(BCRdt$PROVINCE) # Names in the dataset
plot1PRV <- effectsOfForestryWithTime(fullTableList = finalPixelTableListBCRPROV,
                                      spatialScale = spatialScale,
                                      tableName = "PROVINCE",
                                      pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                          folderForTables,
                                                          file.path(getwd(), "data")),
                                      subsetName = "PROV", birdSpecies = NULL,
                                      calculate = TRUE, namesTabs = namesTabs)
# BCR_PROV
namesTabs <- c(paste(BCRdt$PROVINCE_S, BCRdt$BCR, sep = "_")) # Names you want
names(namesTabs) <- unique(BCRdt$BCR_PROV) # Names in the dataset
plot1BCR_PROV <- effectsOfForestryWithTime(fullTableList = finalPixelTableListBCRPROV,
                                           spatialScale = spatialScale,
                                           tableName = "PROVBCR",
                                           pathToSave = ifelse(pemisc::user() == "tmichele", 
                                                               folderForTables,
                                                               file.path(getwd(), "data")),
                                           subsetName = "PROV_BCR", birdSpecies = NULL,
                                           calculate = TRUE, namesTabs = namesTabs)

# TODO add to this plot the one showing forestry activities!


#####################################################################################

# 2: How much did habitat supply change from 1984 to the given year exclusively due 
# to forestry activities
# (cummulative rate of habitat loss) 

#####################################################################################

# Rate of change on pixel based per year. This means that I don't have a SD
source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/pixelYealyRateOfChangeSummary.R')
manUnmanTable <- pixelYealyRateOfChangeSummary(fullTableList = finalPixelTableList, 
                                               fullTableName = file.path(folderForTables, 
                                                                         paste0("rateChangeManaged",
                                                                                spatialScale,"m.rds")),
                                               subsetName = "value")
quebecTable <- pixelYealyRateOfChangeSummary(fullTableList = finalPixelTableListQuebec, 
                                             fullTableName = file.path(folderForTables, 
                                                                       paste0("rateChangeQuebec",
                                                                              spatialScale,"m.rds")),
                                             subsetName = "value")
AlbertaTable <- pixelYealyRateOfChangeSummary(fullTableList = finalPixelTableListAlberta, 
                                              fullTableName = file.path(folderForTables, 
                                                                        paste0("rateChangeAlberta",
                                                                               spatialScale,"m.rds")),
                                              subsetName = "value")
BCRTable <- pixelYealyRateOfChangeSummary(fullTableList = finalPixelTableListBCRPROV, 
                                          fullTableName = file.path(folderForTables, 
                                                                    paste0("rateChangeBCR",
                                                                           spatialScale,"m.rds")),
                                          subsetName = "BCR")
PROVTable <- pixelYealyRateOfChangeSummary(fullTableList = finalPixelTableListBCRPROV, 
                                           fullTableName = file.path(folderForTables, 
                                                                     paste0("rateChangePROV",
                                                                            spatialScale,"m.rds")),
                                           subsetName = "PROV")
PROVBCRTable <- pixelYealyRateOfChangeSummary(fullTableList = finalPixelTableListBCRPROV, 
                                              fullTableName = file.path(folderForTables, 
                                                                        paste0("rateChangePROVBCR",
                                                                               spatialScale,"m.rds")),
                                              subsetName = "PROV_BCR")

source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/plotYearlyRateOfChange.R')
namesTabs <- c("Managed Forest", "Unmanaged Forest") # Names you want
names(namesTabs) <- c("1", "2") # Names in the dataset
manUnmanPlot1 <-  plotYearlyRateOfChange(table = manUnmanTable, birdSpecies = NULL,
                                         whatToPlot = "abundance", # abundance, cummPerc
                                         namesTabs = namesTabs)

manUnmanPlot2 <-  plotYearlyRateOfChange(table = manUnmanTable, birdSpecies = NULL,
                                         whatToPlot = "cummPerc", # abundance, cummPerc
                                         namesTabs = namesTabs)

namesTabs <- c("QUEBEC") # Names you want
names(namesTabs) <- c("1") # Names in the dataset
quebecPlot <-  plotYearlyRateOfChange(table = quebecTable, birdSpecies = NULL,
                                      whatToPlot = "cummPerc", # abundance, cummPerc
                                      namesTabs = namesTabs)

namesTabs <- c("ALPAC") # Names you want
names(namesTabs) <- c("1") # Names in the dataset
albertaPlot <-  plotYearlyRateOfChange(table = AlbertaTable, birdSpecies = NULL,
                                       whatToPlot = "cummPerc", # abundance, cummPerc
                                       namesTabs = namesTabs)

namesTabs <- c(paste0("BCR", unique(BCRdt$BCR))) # Names you want
names(namesTabs) <- unique(BCRdt$BCR) # Names in the dataset
BCRPlot <-  plotYearlyRateOfChange(table = BCRTable, birdSpecies = NULL,
                                   whatToPlot = "cummPerc", # abundance, cummPerc
                                   namesTabs = namesTabs, direction = "vertical")

namesTabs <- unique(BCRdt$PROVINCE_S) # Names you want
names(namesTabs) <- unique(BCRdt$PROVINCE) # Names in the dataset
PROVPlot <-  plotYearlyRateOfChange(table = PROVTable, birdSpecies = NULL,
                                    whatToPlot = "cummPerc", # abundance, cummPerc
                                    namesTabs = namesTabs, direction = "vertical")

#####################################################################################

# 3. Which species did we loose most? Are these the most abundant ones?  

#####################################################################################

source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/plotOfLostSpecies.R')
namesTabs <- c("Managed Forest", "Unmanaged Forest") # Names you want
names(namesTabs) <- c("1", "2") # Names in the dataset
managedLostPlot <- plotOfLostSpecies(table = manUnmanTable, 
                                     namesTabs = namesTabs)

namesTabs <- c("QUEBEC") # Names you want
names(namesTabs) <- c("1") # Names in the dataset
quebecPlot <- plotOfLostSpecies(table = quebecTable, 
                                namesTabs = namesTabs)

namesTabs <- c("ALPAC") # Names you want
names(namesTabs) <- c("1") # Names in the dataset
albertaPlot <-  plotOfLostSpecies(table = AlbertaTable, 
                                  namesTabs = namesTabs)

namesTabs <- c(paste0("BCR", unique(BCRdt$BCR))) # Names you want
names(namesTabs) <- unique(BCRdt$BCR) # Names in the dataset
BCRPlot <-  plotOfLostSpecies(table = BCRTable, 
                              namesTabs = namesTabs)

namesTabs <- unique(BCRdt$PROVINCE_S) # Names you want
names(namesTabs) <- unique(BCRdt$PROVINCE) # Names in the dataset
PROVPlot <-  plotOfLostSpecies(table = PROVTable, 
                               namesTabs = namesTabs)

#####################################################################################

# 4. Where spatially did disturbance happen? 
# (Map of change in disturbance: mergedFocal2011)

#####################################################################################

mergedFocal2011map <- raster(file.path(getwd(),
                                       "modules/focalCalculation/data",
                                       paste0("mergedFocal2011-", spatialScale,
                                              "Res250m.tif")))

#####################################################################################

# 5. How is disturbance proportion distributed among study areas? 
# (Histograms of disturbance for each place)

#####################################################################################

mergedFocal2011 <- data.table::data.table(pixelID = 1:ncell(mergedFocal2011map),
                                          value = raster::getValues(mergedFocal2011map))
names(mergedFocal2011)[names(mergedFocal2011) == "value"] <- "disturbance"

managedForestDT <- readRDS(file.path(posthocFolder, "managedForestDT.rds"))
names(managedForestDT)[names(managedForestDT) == "value"] <- "managed"

managedForestProvBCR <- readRDS(file.path(posthocFolder, "managedForestProvBCR.rds"))

albertaSADT <- readRDS(file.path(posthocFolder, "albertaSADT.rds"))
names(albertaSADT)[names(albertaSADT) == "value"] <- "Alberta"

quebecSADT <- readRDS(file.path(posthocFolder, "quebecSADT.rds"))
names(quebecSADT)[names(quebecSADT) == "value"] <- "Quebec"

setkey(mergedFocal2011, "pixelID")
setkey(managedForestDT, "pixelID")
setkey(managedForestProvBCR, "pixelID")
setkey(albertaSADT, "pixelID")
setkey(quebecSADT, "pixelID")

MergedDT <- Reduce(function(...) merge(..., all.x = TRUE, by = "pixelID"), list(mergedFocal2011, 
                                                                                managedForestDT,
                                                                                managedForestProvBCR,
                                                                                albertaSADT,
                                                                                quebecSADT))
# Make histograms 
# for (i.e. Alberta's ALPAC, Quebec, Managed and Unmanaged forests)

cols <- c("managed", "PROV", "BCR", "Alberta", "Quebec")
nms <- list("managed" = c("unmanagedForest", "managedForest"), 
            "PROV" = c("Yukon", "NWT", 
                       "BC", "Alberta", "Newfoundland", 
                       "Quebec", "Saskatchewan", 
                       "Manitoba", "Ontario"), 
            "BCR" = c("BCR4", "BCR6", "BCR3", 
                      "BCR7", "BCR5", "BCR10", 
                      "BCR8", "BCR11", "BCR14", 
                      "BCR12"), 
            "Alberta" = "ALPAC", 
            "Quebec" = "QuebecSA")

vecList <- lapply(cols, function(col){
  vec <- MergedDT[!is.na(get(col)), disturbance, by = col]
  vecList <- lapply(unique(vec[[col]]), function(each){
    vecInternal <- vec[get(col) == each, disturbance]
    vecInternal <- vecInternal[!is.na(vecInternal)]
    return(vecInternal)
  })
  names(vecList) <- paste0(col, unique(vec[[col]]))
  return(vecList)
})
names(vecList) <- cols

vecList <- lapply(seq_along(vecList), function(i){
  names(vecList[[i]]) <- nms[[i]]
  return(vecList[[i]])
})
names(vecList) <- cols

colorTable <- data.table(region = c("unmanagedForest", "managedForest", 
                                    "Yukon", "NWT","BC", "Alberta", "Newfoundland", 
                                    "Quebec", "Saskatchewan", "Manitoba", "Ontario", 
                                    "BCR4", "BCR6", "BCR3",
                                    "BCR7", "BCR5", "BCR10", 
                                    "BCR8", "BCR11", "BCR14", 
                                    "BCR12", 
                                    "ALPAC", "QuebecSA"),
                         color = c("darkgreen", "forestgreen", "purple3", 
                                   "orchid2", "lightslateblue", "steelblue2", 
                                   "seagreen2", "greenyellow", "gold2", 
                                   "darkorange2", "firebrick2", 
                                   "purple3", 
                                   "orchid2", "lightslateblue", "steelblue2", 
                                   "seagreen2", "greenyellow", "gold2", 
                                   "darkorange2", "orangered2", 
                                   "firebrick2",
                                   "darkolivegreen", "palegreen2")
)

dt <-  rbindlist(lapply(names(vecList), FUN = function(outter){
  each <- rbindlist(lapply(names(vecList[[outter]]), function(inner){
    dt <- data.table("disturbance" = vecList[[outter]][[inner]],
                     region = inner, 
                     groupping = outter,
                     color = colorTable[region == inner, color])
    return(dt)
  })
  )
  return(each)
})
)
names(dt) <- names(vecList)

# WITH ZEROS
plots <- lapply(unique(dt$groupping), function(SA){
  dtSub <- as.data.frame(dt[groupping == SA])
  p <- ggplot(data = dtSub, aes(x = disturbance, fill = region)) +
    geom_histogram(position = "dodge",
                   data = dtSub, breaks = seq(0, 1, by = 0.05),
                   fill = rep(unique(dtSub$color), 
                              each = length(seq(0, 1, by = 0.05))-1)) +
    labs(title = "Disturbance in the Boreal Forest", 
         x = "Proportion of disturbance", 
         y = "Number of pixels") +
    theme_linedraw() +
    theme(legend.position = "bottom") +
    facet_grid(. ~ region)
  plotName <- file.path(posthocFolder, paste0(SA, ".png"))
  png(plotName, width = 900, height = 400)
  print(p)
  dev.off()
  return(plotName)
})
library(googledrive)
lapply(plots, drive_upload, as_id("1ohwCui7wJLoGImlzzoQCtNiJjWhmVS39"))

# Without ZEROS

plots <- lapply(cols, function(SA){
  dtSub <- as.data.frame(dt[groupping == SA & disturbance > 0])
  p <- ggplot(data = dtSub, aes(x = disturbance, fill = region)) +
    geom_histogram(position = "dodge",
                   data = dtSub, breaks = seq(0, 1, by = 0.05),
                   fill = rep(unique(dtSub$color), 
                              each = length(seq(0, 1, by = 0.05))-1)) +
    labs(title = "Disturbance in the Boreal Forest -- Zeros Removed", 
         x = "Proportion of disturbance", 
         y = "Number of pixels") +
    theme_linedraw() +
    theme(legend.position = "bottom") +
    facet_grid(. ~ region)
  plotName <- file.path(posthocFolder, paste0(SA, "no0.png"))
  png(plotName, width = 900, height = 400)
  print(p)
  dev.off()
  return(plotName)
})
library(googledrive)
lapply(plots, drive_upload, as_id("1ohwCui7wJLoGImlzzoQCtNiJjWhmVS39"))


#####################################################################################

# 6. What is the proportion of disturbed and undisturbed areas, amnd ? 
# (Calculate the exact area disturbed and not disturbed for each polygon)

#####################################################################################

distDT <- dt[disturbance > 0, .N, by = c("region", "groupping")]
names(distDT)[names(distDT) == "N"] <- "disturbedPixels"
undistDT <- dt[disturbance  == 0, .N, by = c("region", "groupping")]
names(undistDT)[names(undistDT) == "N"] <- "undisturbedPixels"
totalPix <- dt[, .N, by = c("region", "groupping")]
names(totalPix)[names(totalPix) == "N"] <- "totalPixels"
distTable <- Reduce(function(...) merge(..., by = c("region", "groupping")), list(distDT,
                                                                                  undistDT,
                                                                                  totalPix))
distTable[, c("propDisturbed", "propUndisturbed") := list(disturbedPixels/totalPixels, 
                                                          undisturbedPixels/totalPixels)]
distTable[, names(distTable)[!names(distTable) %in% c("propDisturbed", 
                                                      "propUndisturbed",
                                                      "region", "groupping")] := NULL]
distTable$region[distTable$region == "managedForest"] <- "managed"
distTable$region[distTable$region == "unmanagedForest"] <- "unmanaged"
distTable$region[distTable$region == "Manitoba"] <- "MT"
distTable$region[distTable$region == "Yukon"] <- "YK"
distTable$region[distTable$region == "Newfoundland"] <- "NL"
distTable$region[distTable$region == "Ontario"] <- "ON"
distTable$region[distTable$region == "Quebec"] <- "QC"
distTable$region[distTable$region == "Saskatchewan"] <- "SK"
distTable$region[distTable$region == "Alberta"] <- "AB"

names(distTable)[names(distTable) == "propDisturbed"] <- "Disturbed"
names(distTable)[names(distTable) == "propUndisturbed"] <- "Undisturbed"

distTablePlot <- melt(distTable, measure.vars = c("Disturbed", 
                                                  "Undisturbed"))

## ADD TO THIS PLOT A LAYER WITH A DOT FOR THE % OF BIRDS LOST!! :) 
# What is the total proportion of birds lost: Managed and Unmanaged Forest
totalAbund1984_managed <- manUnmanTable[region == 1 & year == 1984, sum(abundance)]
totalAbund2011_managed <- manUnmanTable[region == 1 & year == 2011, sum(abundance)]
totalBirdsLost <- totalAbund1984_managed - totalAbund2011_managed
percBirdsLostManaged <- 100*((totalAbund2011_managed - totalAbund1984_managed)/totalAbund1984_managed)

totalAbund1984_unmanaged <- manUnmanTable[region == 2 & year == 1984, sum(abundance)]
totalAbund2011_unmanaged <- manUnmanTable[region == 2 & year == 2011, sum(abundance)]
totalBirdsLost <- totalAbund1984_unmanaged - totalAbund2011_unmanaged
percBirdsLostUnmanaged <- 100*((totalAbund2011_unmanaged - totalAbund1984_unmanaged)/totalAbund1984_unmanaged)

# What about in Quebec?
totalAbund1984_Quebec <- quebecTable[region == 1 & year == 1984, sum(abundance)]
totalAbund2011_Quebec <- quebecTable[region == 1 & year == 2011, sum(abundance)]
totalBirdsLost <- totalAbund1984_Quebec - totalAbund2011_Quebec
percBirdsLostQuebec <- 100*((totalAbund2011_Quebec - totalAbund1984_Quebec)/totalAbund1984_Quebec)

# What about in ALPAC?
totalAbund1984_Alberta <- AlbertaTable[region == 1 & year == 1984, sum(abundance)]
totalAbund2011_Alberta <- AlbertaTable[region == 1 & year == 2011, sum(abundance)]
totalBirdsLost <- totalAbund1984_Alberta - totalAbund2011_Alberta
percBirdsLostAlberta <- 100*((totalAbund2011_Alberta - totalAbund1984_Alberta)/totalAbund1984_Alberta)

# What about Provinces?
totalAbund1984_PROV <- PROVTable[year == 1984, sum(abundance), by = region]
totalAbund2011_PROV <- PROVTable[year == 2011, sum(abundance), by = region]
names(totalAbund1984_PROV)[names(totalAbund1984_PROV) == "V1"] <- "abund1984"
names(totalAbund2011_PROV)[names(totalAbund2011_PROV) == "V1"] <- "abund2011"
provTab <- merge(totalAbund1984_PROV, totalAbund2011_PROV, by = "region")
provTab[, totalBirdsLost := abund1984 - abund2011]
provTab[, percBirdsLost := 100*(-totalBirdsLost/abund1984)]
provNames <- unique(BCRdt[, c("PROVINCE_S", "PROVINCE")])
names(provNames)[names(provNames) == "PROVINCE"] <- "region"
provTab <- merge(provTab, provNames, all.x = TRUE, by = "region")
provTab[, names(provTab)[!names(provTab) %in% c("percBirdsLost", "PROVINCE_S")] := NULL]
provTab$provShort <- c("MT", "NL", "NWT", "QC", 
                       "YK", "BC", "AB", "SK", "ON")

# What about BCR?
totalAbund1984_BCR <- BCRTable[year == 1984, sum(abundance), by = region]
totalAbund2011_BCR <- BCRTable[year == 2011, sum(abundance), by = region]
names(totalAbund1984_BCR)[names(totalAbund1984_BCR) == "V1"] <- "abund1984"
names(totalAbund2011_BCR)[names(totalAbund2011_BCR) == "V1"] <- "abund2011"
bcrTab <- merge(totalAbund1984_BCR, totalAbund2011_BCR, by = "region")
bcrTab[, totalBirdsLost := abund1984 - abund2011]
bcrTab[, percBirdsLost := 100*(-totalBirdsLost/abund1984)]
names(bcrTab)[names(bcrTab) == "region"] <- "BCR"
bcrTab[, names(bcrTab)[!names(bcrTab) %in% c("percBirdsLost", "BCR")] := NULL]
knitr::kable(bcrTab)

birdTab <- data.table(region = c("managed", "unmanaged", "ALPAC",
                                 provTab$provShort, bcrTab$BCR,
                                 "QuebecSA"),
                      groupping = c("managed", "managed", "Alberta",
                                    rep("PROV", times = length(provTab$provShort)),
                                    rep("BCR", times = length(bcrTab$BCR)),
                                    "Quebec"), 
                      birds = c(percBirdsLostManaged, percBirdsLostUnmanaged, 
                                percBirdsLostAlberta,
                                provTab$percBirdsLost, bcrTab$percBirdsLost,
                                percBirdsLostQuebec))

setkey(distTablePlot, region, groupping)
setkey(birdTab, region, groupping)

dtPlot <- merge(distTablePlot, birdTab)

dtPlot$region <- factor(dtPlot$region, levels = c("unmanaged", "managed", "ALPAC",
                                                  "NWT", "YK", "BC", "MT","SK",
                                                  "NL", "AB","ON", "QC",
                                                  "BCR7", "BCR4", "BCR11", "BCR5", "BCR10",
                                                  "BCR6", "BCR8", "BCR12", "BCR14", "QuebecSA"))
dtPlot$groupping <- factor(dtPlot$groupping, levels = c("managed", "Alberta", "PROV", "BCR","Quebec"))

names(dtPlot)[names(dtPlot) == "value"] <- "proportion"
dtPlot$birds <- -dtPlot$birds

p <- ggplot(data = dtPlot, aes(fill = variable, y = proportion, x = region)) + 
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("darkred", "darkgreen")) +
  facet_grid(groupping ~ ., labeller = labeller(groupping = namesTabs)) +
  theme_linedraw() + theme(legend.position = "right") + 
  geom_point(aes(y = birds/10), colour = "black", shape = 8) +
  labs(y = "Birds (black stars) = proportional loss * 10\n Disturbance (bars) = proportion")
