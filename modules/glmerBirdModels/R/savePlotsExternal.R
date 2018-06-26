# Save plots

png(file.path(outputPath(mySimOut), "plotAbundanceDisturbance.png"), width = 1500, height = 863) # Modified on 1st June
mySimOut$plotAbundDist
dev.off()

png(file.path(outputPath(mySimOut),"plotCoefficients.png"), width = 1500, height = 863)
mySimOut$plotCoeff
dev.off()

png(file.path(outputPath(mySimOut),"plotDisturbanceSector.png"), width = 1500, height = 863)
mySimOut$plotDistSec
dev.off()