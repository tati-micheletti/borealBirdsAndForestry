defineModule(sim, list(
  name = "hindcastingPosthoc",
  description = paste0("This is a posthoc module designed for the hindcasting project (Micheletti et al., in prep). ",
                       "This, however, can be used by anyone that can provide the input objects described below", 
                       "In this module, a list of species, each containing a data.table with the following columns:", 
                       "pixelID and YearXXXX; where YearXXXX is the density (NOT ABUNDANCE!) of the species of bird", 
                       " in a given year, columns indicating the proportion of density change and the cumulative density",
                       " change are created on the table 'predictedDT', and plots and summaries are created for shapefiles", 
                       "provided by the user (or the default shapefiles, which include canadian provinces, 3 specific study areas)", 
                       " and the managed and unmanaged forests of Canada"),
  keywords = "",
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0.9004", hindcastingPosthoc = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "hindcastingPosthoc.Rmd")),
  reqdPkgs = list("reproducible", "raster", "data.table", "usefulFuns", "magrittr", "rgeos", "sp", "rgdal"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "rasterToMatch", objectClass = "RasterLayer", 
                 desc = "Raster layer that MATCHES the layer used to create the birds table -- as the table contains pixelID", 
                 sourceURL = "https://drive.google.com/open?id=1c-rnifCpK-qU9fkg__f4RP0ueRUNjjiR"),
    expectsInput(objectName = "studyArea", objectClass = "SpatialPolygonsDataframe", 
                 desc = "This is the study area used for fitting the bird models -- defaults to Canadian Boreal Forest", 
                 sourceURL = NA),
    expectsInput(objectName = "predictedDT", objectClass = "list", 
                 desc = paste0("This is a list of species, each containing a data.table with the following columns:", 
                               "pixelID and YearXXXX; where YearXXXX is the density (NOT ABUNDANCE!) of the species of bird", 
                               " in a given year"), 
                 sourceURL = NA),
    expectsInput(objectName = "shapefilesList", objectClass = "list", 
                 desc = paste0("This is a list of data.tables containing 2 columns: pixelID and shape",
                               " The last is a character class vector, with the identification of the given pixel for",
                               " summary purposes. Please not that the pixelID needs to come from a shapefile that",
                               " had been projected to rasterToMatch"), 
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

doEvent.hindcastingPosthoc = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      scipOrig <- getOption("scipen")
      options(scipen = 10000)
      on.exit(options(scipen = scipOrig))
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "hindcastingPosthoc", "createCumulativeRate")
      sim <- scheduleEvent(sim, time(sim), "hindcastingPosthoc", "plotEffectsOfForestryThroughTime")
      sim <- scheduleEvent(sim, time(sim), "hindcastingPosthoc", "pixelYealyRateOfChangeSummary")
      sim <- scheduleEvent(sim, time(sim), "hindcastingPosthoc", "plotYearlyRateOfChange")
      sim <- scheduleEvent(sim, time(sim), "hindcastingPosthoc", "plotOfLostSpecies")
      sim <- scheduleEvent(sim, time(sim), "hindcastingPosthoc", "plotWhereDisturbanceHappened")
      sim <- scheduleEvent(sim, time(sim), "hindcastingPosthoc", "disturbanceProportionDistributed")
      sim <- scheduleEvent(sim, time(sim), "hindcastingPosthoc", "plotDisturbedAndUndisturbedWithBirds")
      
    },
    createCumulativeRate = {
      browser()
      # 4.2 RATE YEARS 1984 - 2011 PER BIRD
      # 4.2.1 Rate per year: rate
      fullTableList <- readRDS(fullTablePixels[[bird]]) # CALCULATED TABLE WITH CHANGES
      dcastedTable <- dcast(data = fullTableList, formula = species + pixelID ~ year, value.var = "abundance")
      ys <- usefun::substrBoth(unique(fullTableList$year), howManyCharacters = 4, fromEnd = TRUE)
      newNames <- c(names(dcastedTable)[1:2], paste0("abund", ys))
      names(dcastedTable) <- newNames
      fullTableList <- dcastedTable
      abundNames <- usefun::grepMulti(names(fullTableList), patterns = c("abund"))
      envir <- environment()
      invisible(lapply(2:length(abundNames), function(i){
        t1 <- Sys.time()
        yearRef <- usefulFuns::substrBoth(strng = abundNames[i],
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
        browser()
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
    },
    plotEffectsOfForestryThroughTime = {

      #####################################################################################
      
      # 1. How do I know if the general effects of forestry on birds are reducing through 
      # time? If the 'rate' of change from one year is smaller than the previous: 
      # (birdAbundanceInYear1-birdAbundanceInYear0)/birdAbundanceInYear0
      
      #####################################################################################
      
      browser()
      
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
      
    },
    pixelYealyRateOfChangeSummary = {
      browser()
      #####################################################################################
      
      # 2: How much did habitat supply change from 1984 to the given year exclusively due 
      # to forestry activities
      # (cummulative rate of habitat loss) 
      
      #####################################################################################
      
      # Rate of change on pixel based per year. This means that I don't have a SD
      source(file.path(getwd(), 'functions/pixelYealyRateOfChangeSummary.R'))
      manUnmanTable <- pixelYealyRateOfChangeSummary(fullTableList = finalPixelTableList, 
                                                     fullTableName = file.path(folderForTables, 
                                                                               paste0("rateChangeManaged",
                                                                                      spatialScale,"m.rds")),
                                                     subsetName = "value")
    },
    plotYearlyRateOfChange = {
      browser()
      source(file.path(getwd(), '/functions/plotYearlyRateOfChange.R'))
      namesTabs <- c("Managed Forest", "Unmanaged Forest") # Names you want
      names(namesTabs) <- c("1", "2") # Names in the dataset
      manUnmanPlot1 <-  plotYearlyRateOfChange(table = manUnmanTable, birdSpecies = NULL,
                                               whatToPlot = "abundance", # abundance, cummPerc
                                               namesTabs = namesTabs, 
                                               direction = "vertical")
      
    },
    plotOfLostSpecies = {
      browser()
      #####################################################################################
      
      # 3. Which species did we loose most? Are these the most abundant ones?  
      
      #####################################################################################
      
      source(file.path(getwd(), 'functions/plotOfLostSpecies.R'))
      namesTabs <- c("Managed Forest", "Unmanaged Forest") # Names you want
      names(namesTabs) <- c("1", "2") # Names in the dataset
      managedLostPlot <- plotOfLostSpecies(table = manUnmanTable, 
                                           namesTabs = namesTabs, 
                                           plotScatter = TRUE, 
                                           direction = "vertical")
    },
    plotWhereDisturbanceHappened = {
      browser()
      #####################################################################################
      
      # 4. Where spatially did disturbance happen? 
      # (Map of change in disturbance: mergedFocal2011)
      
      #####################################################################################
      
      mergedFocal2011map <- raster(file.path(getwd(),
                                             "modules/focalCalculation/data",
                                             paste0("mergedFocal2011-", spatialScale,
                                                    "Res250m.tif")))
      
    },
    plotDisturbanceProportionDistributed = {
      browser()
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
      
      
    },
    plotDisturbedAndUndisturbedWithBirds = {
      browser()
      #####################################################################################
      
      # 6. What is the proportion of disturbed and undisturbed areas, and bird proportion 
      # change? 
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
      
    }, 
    
    ######################################################################################
    # COULD POTENTIALLY ADD MORE MAPS FROM paperPlots.R --> Do it once building the module
    ######################################################################################    
    
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  if (!suppliedElsewhere("studyArea", sim)){
    # Default = Canadian boreal
    sim$studyArea <- Cache(defineStudyArea, testArea = TRUE, 
                              specificTestArea = "boreal", 
                              mapSubset = "Canada")
  }
  if (!suppliedElsewhere("birdSpecies", sim)){
    sim$birdSpecies = c("BBWA", "BLPW", "BOCH", "BRCR",
                        "BTNW", "CAWA", "CMWA","CONW",
                        "OVEN", "PISI", "RBNU", "SWTH",
                        "TEWA", "WETA", "YRWA")
  }
  if (!suppliedElsewhere("rasterToMatch", sim)){
    # Default = this rasterToMatch that matches data coming from bayesianBirdModel
    sim$rasterToMatch <- prepInputs(url =  "https://drive.google.com/open?id=1c-rnifCpK-qU9fkg__f4RP0ueRUNjjiR",
                                    targetFile = "rasterToMatch_fromFocalRasters.qs",
                                    destinationPath = inputPath(sim),
                                    studyArea = sim$studyArea, fun = "qs::qread")
  }
  if (!suppliedElsewhere("shapefilesList", sim)){
    # Default = has small study areas in QC, AB, AB, Provinces and Managed Boreal Forest
    sim$shapefilesList <- getDefaultShapefiles(rasterToMatch = sim$rasterToMatch, # Cache(
                                               studyArea = sim$studyArea, 
                                               pathData = dPath)#,
                               #  useCache = "overwrite",
                               # userTags = c(cacheTags, "function2:getDefaultShapefiles",
                               #             "objectName:shapefilesList"))
  }
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
