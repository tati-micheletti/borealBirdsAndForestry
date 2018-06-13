splitRasterAndPredict <- function(inputSpecies = sim$inputSpecies,
                                  models = sim$models,
                                  birdDensityRasters = sim$birdDensityRasters){
  
  populationTrends <- Map(f = groupSplitRaster, spec = inputSpecies, mod = models, abund = birdDensityRasters,  
           MoreArgs = list(sim = sim))
  names(populationTrends) <- inputSpecies
  
  return(populationTrends)
  
}

