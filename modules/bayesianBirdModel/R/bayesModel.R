# Observations
#   Not sure negative binomial is the best one… Maybe ZIP would be better? 
#   We can try extra-Poisson dispersion model (ie. 12.3.3. Kery and Schaub 2012), 
#   negative binomial (Kery and Royle 2016) or even regular Poisson (depends really 
#   on the species we are dealing with… I don’t think Poisson is the best for old-forest 
#   associated birds!).
#   
#   We could eventually model time (HH MM) as RE of propability of observation

# Obvious outcomes
#   NN > NL

#   # Assumptions
# 1. Bird abundance might vary depending on cluster (RE)
# 2. Bird abundance might vary depending on the year (RE)


bayesModel <- function(birdData = sim$birdData, 
                       birdSpecies = sim$birdSpecies, 
                       ageMap = sim$ageMap,
                       beads = sim$beads,
                       dataPath = dataPath(sim),
                       rP = sim$rP){
  
  
  # Subsetting to avoid using data from other types of disturbance
  birdDataN <- birdData[Agent_500=="Forestry" | Agent_500==""]
  birdDataFinal <- birdDataN[Agent_100=="Forestry" | Agent_100==""]
  length(unique(birdDataFinal$SS))-nrow(birdDataFinal) # How many sites have been revisited in different years? # up to 2775, but likely less as many have 3 years of visits
  
  if(!is.null(rP)){ # If we have a test study area
  
    # Crop data Already have a code for that
    BDFcoor <- birdDataFinal
    coordinates(BDFcoor)=~X+Y
    proj4string(BDFcoor)<- CRS("+proj=longlat +datum=WGS84") # Seems to be aligning, confirm data is only for boreal forest
    
    #  birdShape <- SpaDES.tools::postProcess(x = BDFcoor, rasterToMatch = ageMap) #Failing silently! Need to fix it! [ FIX ]
    birdShape <- spTransform(BDFcoor, CRSobj = crs(beads))
    birdShape <- maskInputs(x = birdShape, studyArea = rP) # NOT ALIGNING PERFECTLY... WHY??? Still should be good enough to continue...? 
    
    # Crop and mask ageMap
    croppedAgeMap <- crop(ageMap, rP) %>%
      mask(rP)
    croppedAgeMap[is.na(croppedAgeMap)] <- -9999
    ageMasked <- mask(croppedAgeMap, mask = rP)
    ageMasked[is.na(ageMasked)] <- -5555
    ageMasked[ageMasked<(-7777)] <- NA
    matrixAge <- focalWeight(ageMasked, 3000, type = 'circle')
    
    anyNA <- any(is.na(ageMasked[]))
    
    while (!anyNA==FALSE){
      ageMasked <- focal(x = ageMasked, w = matrixAge, 
                         NAonly = TRUE, na.rm = TRUE)
      anyNA <- any(is.na(ageMasked[]))
      message("NA's still present, iterating again...")
    }
    
    ageMasked[ageMasked<0] <- 0
    ageMasked <- mask(ageMasked, rP)
    
  } else {   # If we want to run the full spatial extention
  
    # Creating a Spatial object from BAM points
    BDFcoor <- birdDataFinal
    
    # Cropping and masking age map
    canadaShapefile <- raster::getData("GADM", country="CAN", level=1,
                                path = dataPath) %>%
      spTransform(CRSobj = crs(ageMap))
    borealShape <- prepInputs(targetFile = file.path(dataPath, "NABoreal.shp"), 
                              url = "http://cfs.nrcan.gc.ca/common/boreal.zip", 
                              destinationPath = dataPath, 
                              rasterToMatch = ageMap, studyArea = canadaShapefile)
    borealShp <- borealShape[borealShape$TYPE=="BOREAL"|borealShape$TYPE=="HEMIBOREAL",]
    croppedAgeMap <- Cache(crop, ageMap, borealShape) %>%
      Cache(mask, ., mask = borealShape)
    
    croppedAgeMap[is.na(croppedAgeMap)] <- -9999
    ageMasked <- mask(croppedAgeMap, mask = borealShape)
    ageMasked[is.na(ageMasked)] <- -5555
    ageMasked[ageMasked<(-7777)] <- NA
    matrixAge <- focalWeight(ageMasked, 3000, type = 'circle')
    
    anyNA <- any(is.na(ageMasked[]))
    
    while (!anyNA==FALSE){
      ageMasked <- focal(x = ageMasked, w = matrixAge, 
                         NAonly = TRUE, na.rm = TRUE)
      anyNA <- any(is.na(ageMasked[]))
      message("NA's still present, iterating again...")
    }
    
    ageMasked[ageMasked<0] <- 0
    ageMasked <- mask(ageMasked, borealShape)
    
    coordinates(BDFcoor)=~X+Y
    proj4string(BDFcoor)<- CRS("+proj=longlat +datum=WGS84") # Seems to be aligning, confirm data is only for boreal forest
    
    #  birdShape <- SpaDES.tools::postProcess(x = BDFcoor, rasterToMatch = ageMap) #Failing silently! Need to fix it! [ FIX ]
    birdShape <- spTransform(BDFcoor, CRSobj = crs(beads))
  }
  
  # Adding forest age to the dataset 
  age <- extract(ageMasked, birdShape)
  ageMapDF <- data.frame(X = as.numeric(coordinates(birdShape)[,1]), 
                         Y = as.numeric(coordinates(birdShape)[,2]), 
                         Year = birdShape@data$YYYY, Age2004 = age)
  ageMapDF$correctedAge <- (ageMapDF$Year - 2004) + ageMapDF$Age2004
  
  # Check for stations that ended up with the same X and Y (due to proximity)
  dup <- which(duplicated(ageMapDF[,1:3]))
  ageMapDF[dup,1] <- ageMapDF[dup,1] + 0.000001 # adding a slight change of position to avoid overlap of stations
  
  # Model dataframe
  dfModel <- lapply(X = birdSpecies, FUN = function(x){
    data.frame(site = paste(ageMapDF[,1], ageMapDF[,2], sep = ":"),
               X = ageMapDF[,1],
               Y = ageMapDF[,2],
               percDistL = birdShape@data$State_P_100,
               percDistN = birdShape@data$State_P_500,
               age = ageMapDF$correctedAge,
               estDensity = eval(parse(text = paste0("birdShape@data$LOG_BCR_", x))),
               offset = eval(parse(text = paste0("birdShape@data$OF_", x))),
               Cluster = birdShape@data$ClusterSP,
               Year = birdShape@data$YYYY,
               counts = eval(parse(text = paste0("birdShape@data$AB_", x))))
    }
  )
  names(dfModel) <- birdSpecies
  
  # Counts dataframe
  dfCounts <- lapply(dfModel, function(x){
    dfCounts <- x[, c(1, 10, 11)]
    return(dfCounts)
   }
  )
  names(dfCounts) <- birdSpecies
  
  dfCounts <- lapply(dfCounts, function(x){
    dcast(x, site ~ Year)
  })
  names(dfCounts) <- birdSpecies
  
  browser()
  
  toCheck <- 54873.7227316193:77760.0004461716 # This doesn't seem to exist?! I need to check if it is casting ok. I need a count per year.
  dfCounts$BBWA[dfCounts$BBWA$site==toCheck,]
    
  # Extract year sequence of data: THIS ARE MY COLUMNS IN COUNT DF
  ys <- sort(unique(ageMapDF$Year))
  # site <- paste(ageMapDF["X"], ageMapDF["Y"], sep = ":")

  # Design matrix for abundance model (no intercept)
#   modelDF <- lapply(birdSpecies, function(x) {
# 
#     spDF <- eval(parse(text = paste0("dfModel$", x)))
#     
#     # identifying duplicates (repeated visited sites): CREATING COUNTS DATAFRAME
#     dup <- spDF[duplicated(spDF[,'site']), 'site']
#     ext <- spDF[,'site']
#     rows <- which(ext %in% dup)
#     spDF2 <- spDF[rows,c('site','Year','counts')] %>%
#       dcast(site ~ Year)
#     
#     
#     browser()
#     
#     
# return()
#     
#   })
  
  # AGAIN LAPPLY THROUGH SPECIES.... <~~~~~~~~~ FROM HERE
    attach(spDF)
    N.neig <- stats::model.matrix(~ percDistN + (1-percDistN) * age + log(estDensity))[,-1]
    N.loc <- stats::model.matrix(~ percDistL + (1-percDistL) * age)[,-1]
    N.locObs <- stats::model.matrix(~ percDistL + (1-percDistL) * age + percDistN + (1-percDistN) * age + log(estDensity))[,-1]
    switchYear <- 1 # or 0 to turn off random effects of Year
    switchCluster <- 1 # or 0 to turn off random effects of Cluster
    
    # Initial values
    phi <- rbinom(1, 1, 0.5)
    sd.cluster <- runif(1, 0, 1)
    sd.year <- runif(1, 0, 1)
    beta0N <- rnorm(1, 0, 0.1)
    beta0L <- rnorm(1, 0, 0.1)
    alpha0 <- rnorm(1, 0, 0.1)
    betaN <- betaL <- alpha <- numeric()
    for (k in 1:3) {  # regression params in lambda N
      betaN[k] <- rnorm(1, 0, 1)}
    for (k in 1:2) {  # regression params in lambda L
      betaL[k] <- rnorm(1, 0, 1)}
    for (k in 1:5) {  # regression params in lambda L
      alpha[k] <- rnorm(1, 0, 1)}
    
    nyears <- sort(unique(Year))
    
    inits <- function(){ list(phi = phi, beta0N = beta0N, beta0L = beta0L, 
                              alpha0 = alpha0, betaN = betaN, sd.year = sd.year,
                              betaL = betaL, alpha = alpha, sd.cluster = sd.cluster,
                              N.neig = N.neig,
                              N.loc = N.loc,
                              offset = offset, 
                              Cluster = Cluster,
                              switchCluster = switchCluster, 
                              Year = Year,
                              switchYear = switchYear,
                              N.locObs = N.locObs,
                              counts = counts) }
    # Parameters monitored
    params <- c("theta", "ltheta", "phi", "beta0", "beta", "sd.lam", "alpha0", "mean.p",
                "alpha", "sd.p.site", "sd.p.survey", "fit.actual", "fit.sim", "bpv", "c.hat",
                "Ntotal263")
    
    # ~~~~~~~ UNTIL HERE STILL TO MODIFY ~~~~~~~~~ #      
    
    return(baysModels) 
}

