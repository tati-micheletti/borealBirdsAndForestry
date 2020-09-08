# tileList <- tiles$Raster1
# library(raster)
# library(data.table)
# totalPixels <- lapply(seq_along(tileList), function(rasN){
#   ras <- tileList[[rasN]]
#   # bring raster to memory
#   ras[] <- ras[]
#   tb <- table(ras[])
#   dt <- data.table(typeDisturbance = as.numeric(names(tb)),
#                    totalPixels = as.numeric(tb),
#                    tile = paste0("tile", rasN))
#   dt <- dt[typeDisturbance != 0, ]
#   print(paste0("Table Finished for tile ", rasN))
#   return(dt)
# })
# tb <- copy(rbindlist(totalPixels))

library("SpaDES")
library("raster")

workDirectory <- getwd()
message("Your current temporary directory is ", tempdir())
tempFolder <- reproducible::checkPath(file.path(dirname(workDirectory), "tmp"), 
                                             create = TRUE)
unixtools::set.tempdir(tempFolder)
rasterOptions(default = TRUE)
options(rasterTmpDir = tempFolder)

SpaDES.core::setPaths(modulePath = file.path(getwd(), "modules"),
                      inputPath = file.path(getwd(), "inputs"),
                      outputPath = file.path(getwd(), "outputs/posthocAnalysis/GIS/"),
                      cachePath = file.path(getwd(), "cache"))

tb <- readRDS(file.path(getwd(), "outputs/tableDisturbanceArea.rds"))
tb2 <- tb[, sum(totalPixels), by = "typeDisturbance"]
names(tb2) <- c("typeDisturbance", "nPixels")
pixelArea <- 30*30
tb2$areaDisturbed_inM2 <- tb2$nPixels*pixelArea
tb2$areaDisturbed_inHa <- tb2$areaDisturbed_inM2*0.0001

# Calculate total area of managed boreal forest
# 1. Crop LCC2005 with boreal managed and Both forest shapefile (yes, not ideal as LCC05 is 250m res, but close enough)
posthocFolder <- file.path(getwd(),"outputs/posthocAnalysis")
maskedDensityRasFolder <- file.path(posthocFolder, "maskedDensityRas")
RTM <- raster(file.path(maskedDensityRasFolder, "densityBBWA.tif"))
source(file.path(getwd(), 'functions/makeBCRandLCC.R'))
pathData <- file.path(getwd(), "modules/birdDensityBCR_Prov_LCC/data/")
# Already just boreal region
BCRLCC05 <- Cache(makeBCRandLCC, 
                  pathData = pathData,
                  RTM = RTM,
                  userTags = c("objectName:BCRLCC05",
                               "script:paperPlots"), 
                  omitArgs = c("overwrite", "userTags", "useCache"))
LCC05 <- BCRLCC05[["LCC05"]]

managedForest <- Cache(prepInputs, url = "https://drive.google.com/open?id=1tgqn8FajD1iSj0aECONGhFzwInau0-q8",
                       targetFile = "NIR2016_MF.shp", archive = "NIR2016_MF.zip",
                       alsoExtract = "similar",
                       studyArea = BCRLCC05$BCR, rasterToMatch = BCRLCC05$LCC05, 
                       overwrite = TRUE, omitArgs = c("overwrite"),
                       destinationPath = file.path(getwd(), "inputs"), 
                       userTags = "objectName:managedForest")
managedForestSF <- sf::st_as_sf(managedForest)
managedForestSF$managed <- c(1, 2)
managedForestRAS <- fasterize::fasterize(sf = managedForestSF, raster = LCC05, field = "managed")
managedForestRAS[managedForestRAS == 0] <- NA
plot(managedForestRAS)

LCCno0 <- LCC05
LCCno0[] <- LCCno0[]
LCCno0[LCCno0 == 0] <- NA
plot(LCCno0, colNA = "red")

# managedForestRAS: 1 == managed; 1&2 == Both
# I can't do separately Both forest because it is not spatially explicit where these disturbances happened 
# (and they did happen almost 100% in the managed forest). 
# However, to have the whole picture, I need to consider both managed AND Both together
# 2. Remove water
# Water = 37:38
noWaterLCC <- LCCno0
noWaterLCC[noWaterLCC %in% c(37, 38)] <- NA
noWaterLCC[!is.na(noWaterLCC)] <- 1
plot(noWaterLCC)
noWaterManaged <- sum(noWaterLCC[managedForestRAS == 1], na.rm = TRUE)
noWaterBoth <- sum(noWaterLCC[managedForestRAS %in% c(1, 2)], na.rm = TRUE)
tb2$noWaterManaged_inHa <- noWaterManaged*(250*250)*0.0001 # NoPixels * area considering resolution * convertion of m2 to ha
tb2$noWaterBoth_inHa <- noWaterBoth*(250*250)*0.0001 # NoPixels * area considering resolution * convertion of m2 to ha

# 3. Remove urban/ice/rocks
  # Ice/snow = 39
  # Rocks = 33
  # Urban = 36
noWURIS <- LCCno0
noWURIS[noWURIS %in% c(33, 36:39)] <- NA
noWURIS[!is.na(noWURIS)] <- 1
plot(noWURIS)
noWURISmanaged <- sum(noWURIS[managedForestRAS == 1], na.rm = TRUE)
noWURISBoth <- sum(noWURIS[managedForestRAS %in% c(1, 2)], na.rm = TRUE)
tb2$noWURISmanaged_inHa <- noWURISmanaged*(250*250)*0.0001 # NoPixels * area considering resolution * convertion of m2 to ha
tb2$noWURISBoth_inHa <- noWURISBoth*(250*250)*0.0001 # NoPixels * area considering resolution * convertion of m2 to ha

# 4. Remove all non-treed areas (Forests = c(1:15, 20, 32))
onlyForest <- LCCno0
onlyForest[!onlyForest %in% c(1:15, 20, 32)] <- NA
onlyForest[!is.na(onlyForest)] <- 1
plot(onlyForest)
onlyForestmanaged <- sum(onlyForest[managedForestRAS == 1], na.rm = TRUE)
onlyForestBoth <- sum(onlyForest[!is.na(managedForestRAS)], na.rm = TRUE)
tb2$onlyForestmanaged_inHa <- onlyForestmanaged*(250*250)*0.0001 # NoPixels * area considering resolution * convertion of m2 to ha
tb2$onlyForestBoth_inHa <- onlyForestBoth*(250*250)*0.0001 # NoPixels * area considering resolution * convertion of m2 to ha

# 1. This is the total in hactares (_inHa) area inside the boreal forest
#    using a given rule: noWater (as WW did), noWURIS (no water, urban, rocks, ice or snow),
#    and only forest (i.e. only pixels considered as forest by LCC05)
# 2. The proportional area (_prop) is what been disturbed by forestry according to WW's layer cropped to the 
#    boreal region AND divided into managed and Both, over the total for each shapefile in 1.
 
tb2$noWaterManaged_prop <- tb2$areaDisturbed_inHa/tb2$noWaterManaged_inHa
tb2$noWaterBoth_prop <- tb2$areaDisturbed_inHa/tb2$noWaterBoth_inHa
tb2$noWURISmanaged_prop <- tb2$areaDisturbed_inHa/tb2$noWURISmanaged_inHa
tb2$noWURISBoth_prop <- tb2$areaDisturbed_inHa/tb2$noWURISBoth_inHa
tb2$onlyForestmanaged_prop <- tb2$areaDisturbed_inHa/tb2$onlyForestmanaged_inHa
tb2$onlyForestBoth_prop <- tb2$areaDisturbed_inHa/tb2$onlyForestBoth_inHa

# Make plot
library(data.table)
tbPlot <- copy(tb2)
tbPlot <- tbPlot[typeDisturbance %in% c(2)] #1, 
tbPlot$typeDisturbance <- c("Forestry") # "Fire"
colsToRemove <- names(tb2)[!names(tb2) %in% c(usefulFuns::grepMulti(names(tb2), pattern = "prop"), 
                                              "typeDisturbance", 
                                              "areaDisturbed_inHa",
                                              usefulFuns::grepMulti(names(tb2), pattern = "inHa"))]
tbPlot[, (colsToRemove) := NULL]
tbPlot <- melt(tbPlot, id.vars = c("typeDisturbance"))
tbPlot$extent <- c("whole boreal forest", rep(c("managed boreal forest", "whole boreal forest"), 
                                              times = (NROW(tbPlot)-1)/2))
tbPlot$forest <- c("total area disturbed", 
                   rep(c("water excluded",
                   "water/urban/rocks/ice/snow excluded", 
                   "only forested pixels"), each = 2, times = (NROW(tbPlot)-1)/2/3))
tbPlot$forest <- factor(tbPlot$forest, levels = c("water excluded", 
                                                  "water/urban/rocks/ice/snow excluded",
                                                  "only forested pixels"))
tbPlot$percentage <- tbPlot$value*100
tbPlot$inPlot <- c(FALSE, rep(c(FALSE, TRUE), each = 6))
tbPlot$Mha <- tbPlot$value/10^6

library(ggplot2)
p <- ggplot(data = tbPlot[inPlot == TRUE, ]) +
  geom_bar(mapping = aes(x = forest, y = percentage, fill = extent), 
           stat = "identity", position = position_dodge2(.9)) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") +
  labs(y = "Percentage (%) of disturbance over total area") + 
  geom_text(data = tbPlot[inPlot == FALSE & variable != "areaDisturbed_inHa", ], 
            aes(x = forest, y = 0.2+tbPlot[inPlot == TRUE, percentage], 
                label = paste0(round(Mha), "Mha")), position = position_dodge2(.9))
p

# From WW 2017: average 0.65 Mha disturbed annually by harvesting (σ= 0.1 Mha) = 17,55 Mha in total. In the boreal forest, 
# we found 9,48ha based on their layers or 54%.

knitr::kable(tb2)

# Comparing Alberto's coefficients to Bayesian Model's
library("usefulFuns")
library("ggplot2")
TRIAL <- "trial8"
destPath <- reproducible::checkPath(file.path(getwd(), "outputs/posthocAnalysis/bayesian", TRIAL), 
                                    create = TRUE)
dt <- readRDS(file.path(destPath, "modelsTableForPlot.rds"))
excl <- grepMulti(as.character(unique(dt$covariate)), patterns = "hyper")
coeffDT <- dt[!covariate %in% excl & model == "bayesian1"]

coeffPlot <- ggplot(coeffDT, aes(x = covariate, y = estimate, 
                                                  ymin = minConf, ymax = maxConf, group = covariate)) +
  geom_point(aes(shape = covariate, color = covariate), size = 5)+#, position = position_dodge(width = 2)) +
  geom_errorbar(aes(color = covariate), width = 0.2) +#, position = position_dodge(width = 2)) +
  facet_grid(. ~ species) + 
  geom_hline(yintercept = 0) +
  theme_gray() +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = ifelse(estimate > 0, estimate + 0.2, estimate - 0.2), 
                label = paste0(round(estimate, 2))), position = position_dodge2(.9),  
            size = 3)
coeffPlot


# Checking out Calling Lake and 



