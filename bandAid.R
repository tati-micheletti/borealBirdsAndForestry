# This script fixes graphs and tables for the paper as a quick fix, but the whole analysis needs to be run again!

# Whole analysis was redone in June 2018. This file is obsolete [ 13th June 2018 ]

# Load Results_List.rds as "MySimOutOld"

# Data that needs updating:
MySimOutOld@.list$data$LocalUndisturbedTransitional <- mySimOut@.envir$data$LocalUndisturbedTransitional
MySimOutOld@.list$data$LocalUndisturbedPermanent <- mySimOut@.envir$data$LocalUndisturbedPermanent

# Data that needs updating:
MySimOutOld@.list$models$LocalUndisturbedTransitional <- mySimOut@.envir$models$LocalUndisturbedTransitional
MySimOutOld@.list$models$LocalUndisturbedPermanent <- mySimOut@.envir$models$LocalUndisturbedPermanent

#Lists and tables that need updating:
source(file = file.path(getwd(), "modules/glmerBirdModels/R", "plotList.R"))
  plotList <- plotList(sim = MySimOutOld, 
                       outputPath = file.path(getwd(),"outputs"), 
                       dataset = MySimOutOld@.list$models, 
                       combinations = MySimOutOld@.list$combinations, 
                       birdSp = MySimOutOld@.list$birdSpecies)

  source(file = file.path(getwd(), "modules/glmerBirdModels/R", "tableSampling.R"))
  tableSampling <- tableSampling(outputPath = file.path(getwd(),"outputs"), 
                                 sim = MySimOutOld, 
                                 dataName = MySimOutOld@.list$dataName, 
                                 dataset = MySimOutOld@.list$data)
  
  source(file = file.path(getwd(), "modules/glmerBirdModels/R", "tableAIC.R"))
  tableAIC <- tableAIC(sim = MySimOutOld, 
                       outputPath = file.path(getwd(),"outputs"), 
                       models = MySimOutOld@.list$models, 
                       combinations = MySimOutOld@.list$combinations, 
                       birdSp = MySimOutOld@.list$birdSpecies)
  
# Graphs that need updating:
  
  source(file = file.path(getwd(), "modules/glmerBirdModels/R", "plotAbundanceDisturbance3.R"))
  plotAbundanceDisturbance <- plotAbundanceDisturbance3(sim = MySimOutOld, 
                       outputPath = file.path(getwd(),"outputs"),
                       plotList = plotList)
  
  source(file = file.path(getwd(), "modules/glmerBirdModels/R", "plotCoefficients3.R"))
  plotCoefficients <- plotCoefficients3(sim = MySimOutOld, 
                                                        outputPath = file.path(getwd(),"outputs"),
                                                        plotList = plotList)
  
