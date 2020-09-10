# Why there are so many NA's in the harvesting layer?
# Are these pixels that were never cut, or smth else like water, cities, or burns?
# Shouldn't these all be zeros and only be NA outside of the studyArea?
reproducibleSHA <- "bcab40a1"
stepCacheTag <- c(paste0("cache:2_simulationSetup"))
harvestingLayer <- reproducible::Cache(reproducible::prepInputs, 
                                     url = paste0(
                                       "https://opendata.nfis.org/downloads/",
                                       "forest_change/CA_forest_harvest_mask",
                                       "_year_1985_2015.zip"),
                                     targetFile = "CA_harvest_year_1985_2015.tif", 
                                     destinationPath = tempdir(),
                                     filename2 = "originalRTMdisturbance",
                                     userTags = c("objectName:rasterToMatch", stepCacheTag,
                                                  "outFun:noCache", paste0("reproducibleCommit:",
                                                                           reproducibleSHA),
                                                  "goal:getCRS"),
                                     omitArgs = c("overwrite", "destinationPath"))
studyArea <- reproducible::Cache(usefulFuns::defineStudyArea,
                                 testArea = TRUE)
Ontario <- postProcess(x = harvestingLayer, studyArea = studyArea, destinationPath = tempdir())

# Check the Edehzhie and compare to the map
library(raster)
plot(Ontario, colNA = "grey")

# Conclusion: the harvesting layer ONLY has values where forestry happened. Therefore
# I need to use the RTM to fix that.