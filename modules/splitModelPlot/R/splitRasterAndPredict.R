splitRasterAndPredict <- function(inputSpecies = sim$inputSpecies,
                                  models = sim$scaleModels,
                                  birdDensityRasters = sim$birdDensityRasters,
                                  disturbanceType = sim$disturbanceType,
                                  disturbanceYear = sim$disturbanceYear,
                                  landCover = sim$landCover,
                                  pathData = dataPath(sim),
                                  nx = P(sim)$nx,
                                  ny = P(sim)$ny,
                                  buffer = P(sim)$buffer,
                                  rType = P(sim)$rType,
                                  start = start(sim),
                                  end = end(sim),
                                  forestClass = P(sim)$forestClass,
                                  focalDistance = P(sim)$focalDistance,
                                  disturbanceClass = P(sim)$disturbanceClass,
                                  intermPath = cachePath(sim),
                                  rP = sim$rP){
  
  populationTrends <- Map(models = models,
                              birdDensityRasters = birdDensityRasters,
                              f = groupSplitRaster,
                              MoreArgs = list(
                                  rP = rP,
                                  disturbanceType = disturbanceType,
                                  disturbanceYear = disturbanceYear,
                                  landCover = landCover,
                                  pathData = pathData,
                                  nx = nx,
                                  ny = ny,
                                  buffer = buffer,
                                  rType = rType,
                                  start = start,
                                  end = end,
                                  forestClass = forestClass,
                                  focalDistance = focalDistance,
                                  disturbanceClass = disturbanceClass,
                                  intermPath = intermPath))
  
  names(populationTrends) <- inputSpecies
  
  return(populationTrends)
  
}

