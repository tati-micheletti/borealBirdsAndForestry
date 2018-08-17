percDensityChange <- deltaAbundanceGraph(populationTrends = mySimOut@.envir$populationTrends, studyArea = mySimOut@.envir$rP, pathData = pathData, outputPath = outputPath)
pathData <- file.path(getwd(), "modules", "finalRasterPlots", "data")
outputPath <- "/home/tmichele/Documents/GitHub/borealBirdsAndForestry/outputs"