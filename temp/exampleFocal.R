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

LCC <- testRas
LCC[LCC == 0] <- 1
LCC[is.na(LCC)] <- 0
raster::plot(LCC)
plot(LCC)

testFocal <- focalWindow(x = testRas, d = 1)

finalRTM <- individualFocal(ras = LCC, 
                            weightMatrix = testFocal, 
                            useInteger = TRUE)

finalRTMwithMask0 <- individualFocal(ras = LCC,
                                     weightMatrix = testFocal,
                                     useInteger = TRUE,
                                     maskTo = c(0, 0))

finalFocal <- individualFocal(ras = testRas,
                              weightMatrix = testFocal,
                              denominatorRaster = finalRTMwithMask0,
                              maskTo = c(NA, NA))

finalRTMwithMaskNA <- individualFocal(ras = RTM,
                                      weightMatrix = testFocal,
                                      useInteger = TRUE,
                                      maskTo = c(0, NA))

finalRTMwithMaskAnd0 <- individualFocal(ras = RTM,
                                        weightMatrix = testFocal,
                                        useInteger = TRUE,
                                        maskTo = c(0, 1, 3))
