bayesModel <- function(birdData = sim$birdData, 
                       birdSpecies = sim$birdSpecies, 
                       ageMap = sim$ageMap,
                       beads = sim$beads){
  
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
  # 
  
  # Subsetting to avoid using data from other types of disturbance
  birdDataN <- birdData[Agent_500=="Forestry" | Agent_500==""]
  birdDataFinal <- birdDataN[Agent_100=="Forestry" | Agent_100==""]
  length(unique(birdDataFinal$SS))-nrow(birdDataFinal) # How many sites have been revisited in different years? # up to 2775, but likely less as many have 3 years of visits
  
  # dissolving beads so it 
  ras <- raster(extent(beads))
  shp <- raster::rasterToPolygons(x = ras)
#  shp.cropped <- raster::crop(x = shp, y = beads)
  
  
  browser() # check alighnment 
  
  BDFcoor <- birdDataFinal
  coordinates(BDFcoor)=~X+Y
  proj4string(BDFcoor)<- CRS("+proj=longlat +datum=WGS84") # Not aligning
  # proj4string(BDFcoor)<- CRS("+proj=longlat +datum=NAD83") # Not aligning
  # proj4string(BDFcoor)<- CRS("+proj=longlat +datum=NAD27") # Not aligning
  birdShape <- SpaDES.tools::postProcess(x = BDFcoor, rasterToMatch = ageMap)
  
  plot(ageMap)
  plot(birdShape, col = "red", add=TRUE)
  clearPlot()
  plot(birdShape, col = "red")
  plot(ageMap, add=TRUE)


  
  age <- raster::getValues(ageMap)
  ageMapDF <- data.frame(X = birdDataFinal$X, Y = birdDataFinal$Y, age = age)
  


  # Model dataframe
  dfModel <- lapply(X = birdSpecies, FUN = function(x){
    data.frame(percDistL = birdDataFinal$State_P_100,
               percDistN = birdDataFinal$State_P_500,
               age = ageMapDf$age,
               estDensity = get(paste0("birdDataFinal$LOG_BCR_", x)),
               offset = get(paste0("birdDataFinal$OF_", x)),
               Cluster = birdData$ClusterSP,
               Year = birdData$YYYY,
               counts = get(paste0("birdDataFinal$AB_", x)))
  })

  # Inputs
  # Sites <- birdDataFinal$SS
  # nsites <- unique()
  # Cluster <- birdDataFinal$SS
  
  # Design matrix for abundance model (no intercept)

  datasetModel <- stats::model.frame()
  N.neig <- stats::model.matrix(~ percDistN + (1-percDistN) * age + log(estDensity))[,-1]
  N.loc <- stats::model.matrix(~ percDistL + (1-percDistL) * age)[,-1]
  N.locObs <- stats::model.matrix(~ percDistL + (1-percDistL) * age + percDistN + (1-percDistN) * age + log(estDensity))[,-1]
  switchYear <- 1 # or 0 to turn off random effects of Year
  switchCluster <- 1 # or 0 to turn off random effects of Cluster
    
# ~~~~~~~ FROM HERE ON STILL TO MODIFY ~~~~~~~~~ #    PAGE 285 from Kery and Royle 2016
    # To monitor: 
    # Initial values
    Nst <- apply(y, 1, max, na.rm = T) + 1
  Nst[is.na(Nst)] <- round(mean(y, na.rm = TRUE))
  Nst[Nst == "-Inf"] <- round(mean(y, na.rm = TRUE))
  inits <- function(){ list(N = Nst, beta0 = 0, mean.p = rep(0.5,3), beta = runif(7, 0,0), alpha
                            = runif(13, 0,0))}
  # Parameters monitored
  params <- c("theta", "ltheta", "phi", "beta0", "beta", "sd.lam", "alpha0", "mean.p",
              "alpha", "sd.p.site", "sd.p.survey", "fit.actual", "fit.sim", "bpv", "c.hat",
              "Ntotal263")
  
  # ~~~~~~~ UNTIL HERE STILL TO MODIFY ~~~~~~~~~ #      
  
  return(baysModels) 
}

