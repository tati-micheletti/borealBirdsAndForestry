library(raster)
source('~/projects/borealBirdsAndForestry/modules/focalStatsCalculation/R/individualFocal.R')
source('~/projects/borealBirdsAndForestry/modules/focalStatsCalculation/R/circularWindow.R')
source('~/projects/borealBirdsAndForestry/modules/focalStatsCalculation/R/focalWindow.R')

testRas <- raster::raster(ncol=5, nrow=5, xmn=0, 
                          xmx=5, ymn=0, ymx=5)
testRas[] <- 0
testRas[c(3:4, 18, 21, 23:24)] <- 1
testRas[c(1:2, 6:7, 11:12, 16)] <- NA
plot(testRas)

RTM <- testRas
RTM[RTM == 0] <- 1
RTM[is.na(RTM)] <- 0
raster::plot(RTM)
plot(RTM)

testFocal <- focalWindow(x = testRas, d = 1)

finalRas <- individualFocal(ras = testRas, 
                            weightMatrix = testFocal, 
                            RTM = RTM)
