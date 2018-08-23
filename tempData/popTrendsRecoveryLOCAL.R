pathData <- file.path(getwd(), "modules", "finalRasterPlots", "data")
outputPath <- "/home/tmichele/Documents/GitHub/borealBirdsAndForestry/outputs"
percDensityChange <- slopeDensityGraph(populationTrends = mySimOut@.envir$populationTrends, studyArea = mySimOut@.envir$rP, pathData = pathData, outputPath = outputPath)
