# Testing for pre2000 assumption
library(reproducible)
library(raster)
library(fasterize)
library(sf)
library(data.table)

# Test data
valsYearO <- sample(x = c(rep(0, times =10), 85:111), size = 100, replace = TRUE)
valsTypeO <- valsYearO
valsTypeO[valsYearO > 0] <- sample(x = c(1:6), size = length(valsTypeO[valsYearO > 0]), replace = TRUE)
valsYear <- valsYearO
valsType <- valsTypeO

# ASSUMPTION: The polygons classified as pre2000 in Alberto's layer 
# BAM_100_BEAD_single_merged (the ones where Year_dist == 1990, excluding 2011 and 2012)
# match disturbances that occur before 2000 in the White and Wulder (2017) layer.

# METHOD: Download both layers, subset and fasterize Alberto's shapefile using WW (as integer),
# subset WW layer to pre 2000 (all up to 1999, the rest becomes 0), integerize WW, stack,
# extract both as vectors to data.table columns (cbind), create 4 columns (WW - Alb):
# Disturbed in both: 0
# Not disturbed in both: 0
# Disturbed in WW and not Alb: 1
# Disturbed in Alb and not in WW: -1

# ANALYSIS:
# Proportion of different pixels over total;
# Correlation: cor(x, y, method = "pearson"); Can be used as we have a big dataset, it approximates phi coefficient;
# Similarity analysis: Jaccard could work to indicate similarity:

jaccard <- function(df, margin) {
  if (margin == 1 | margin == 2) {
    M_00 <- apply(df, margin, sum) == 0
    M_11 <- apply(df, margin, sum) == 2
    if (margin == 1) {
      df <- df[!M_00, ]
      JSim <- sum(M_11) / nrow(df)
    } else {
      df <- df[, !M_00]
      JSim <- sum(M_11) / length(df)
    }
    JDist <- 1 - JSim
    return(c(JSim = JSim, JDist = JDist))
  } else break
}
# Note that margin = 1 (long format)

# 1. Create the folder to make the analysis:
pathToFiles <- file.path(getwd(), "analysisPre2000") %>%
  checkPath(create = TRUE)

# 2. Import layers of disturbance Year and Type (White & Wulder, 2017). Need type to exclude fire!
# This means: exclude 1 and 4. Maybe exclude 3 (low probability of forestry)
# 2 = Forestry, 1 = Fire, 3 and 4 = low probability forestry and fire
disturbanceYear <- prepInputs(url = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Year.zip",
                                    destinationPath = dataPath(sim))
storage.mode(disturbanceYear[]) <- "integer"
disturbanceType <- prepInputs(url = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Type.zip",
                              destinationPath = dataPath(sim))
storage.mode(disturbanceType[]) <- "integer"

# 3. Convert all values of WW that are not of interest to 0
valsYear <- as.integer(getValues(disturbanceYear))
valsType <- as.integer(getValues(disturbanceType))
valsYear[valsType %in% c(1, 4)] <- as.integer(0)
valsYear[valsYear > 99] <- as.integer(0)
valsYear[valsYear != 0] <- as.integer(1)
valsYear <- as.integer(valsYear)

# 4. Manually downloaded zipped all files with the name BAM_100_BEAD_single_merged as "BAM_100_BEAD_single_merged.zip"
# and placed it in this folder

# 4. Used prepInputs to load the shapefile in R
shapeDisturbances <- reproducible::prepInputs(url = "https://drive.google.com/open?id=1Rhii4hchFHFtCOdvtSNPRcnr8kwn3ORg", 
                                              alsoExtract = "similar", destinationPath = pathToFiles, 
                                              rasterToMatch = focalLayer)

# 5. Subset the shapfile as instructed by Alberto: 
# select those points with Year_dist=1990 
shapeDisturbancesSubset <- shapeDisturbances[shapeDisturbances$Year_dist == 1990,]

# remove points YYYY = 2011 and 2012
shapeDisturbancesSubset <- shapeDisturbancesSubset[shapeDisturbancesSubset$YYYY < 2011,]

# 6. Checked if it worked:
unique(shapeDisturbancesSubset@data$Year_dist) # Should have only 1990
unique(shapeDisturbancesSubset@data$YYYY) # Should have all years up to 2010

# 7. Convert the shapefile into sf (faster than shapefile)
shapeDistSF <-sf::st_as_sf(x = shapeDisturbancesSubset)

# 8. Rasterized the shapefile using the raster as base for it:
rasDist <- fasterize::fasterize(sf = shapeDistSF, raster = disturbanceYear)

# 9. Extract the values to a vector
valsDist <- as.integer(getValues(rasDist))

# 10. Stack values in a data.table
dt <- data.table(WW = valsYear, Alb = valsDist)

# 11. Extract the differences:
dt[, dif := WW-Alb]

# 12. Run tests:
corrTest <- cor(dt$WW, dt$Alb)
jaccTest <- jaccard(df = dt[,1:2], margin = 1)

