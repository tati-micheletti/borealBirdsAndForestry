# save plots script

png(file.path(paths$outputPath,"plotCoefficients.png"), width = 1500, height = 863)
mySimOut@.envir$plotCoeff
dev.off()

png(file.path(paths$outputPath, "plotAbundanceDisturbance.png"), width = 1500, height = 863) # Modified on 1st June
mySimOut@.envir$plotAbundDist
dev.off()

png(file.path(paths$outputPath,"plotDisturbanceSector.png"), width = 1500, height = 863)
mySimOut@.envir$plotDistSec
dev.off()

