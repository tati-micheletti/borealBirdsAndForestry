plotModelChecking <- function(species, dt, disturbancePercentage){
  source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/checkModels.R')
  re <- checkModels(useRE = TRUE, species = species, 
                        dt = dt, disturbancePerc = disturbancePercentage, 
                        multiplyByExpectedDensity = FALSE)
  re$dt$type <- "Random Effects"
  re$dt$mult <- "No multiplication"
  noRE <- checkModels(useRE = FALSE, species = species, 
                          dt = dt, disturbancePerc = disturbancePercentage, 
                          multiplyByExpectedDensity = FALSE)
  noRE$dt$type <- "No Random Effects"
  noRE$dt$mult <- "No multiplication"
  reMult <- checkModels(useRE = TRUE, species = species, 
                            dt = dt, disturbancePerc = disturbancePercentage, 
                            multiplyByExpectedDensity = TRUE)
  reMult$dt$type <- "Random Effects"
  reMult$dt$mult <- "Multiplication"
  noREMult <- checkModels(useRE = FALSE, species = species, 
                              dt = dt, disturbancePerc = disturbancePercentage, 
                              multiplyByExpectedDensity = TRUE)
  noREMult$dt$type <- "No Random Effects"
  noREMult$dt$mult <- "Multiplication"
  if (any(!identical(re$distCoeff, reMult$distCoeff), 
          !identical(noRE$distCoeff, noREMult$distCoeff)))
    stop("Something is wrong. The coefficients of the same model shouldn't be non-identical")
  DT <- rbind(re$dt, noRE$dt, reMult$dt, noREMult$dt)
  library("ggplot2")
  p <- ggplot(data = DT, aes(x = x, y = value, col = variable)) +
    geom_line() +
    facet_grid(type ~ mult) +
    theme(legend.position = "bottom") +
    labs(title = paste0(species, " with disturbance = ", disturbancePercentage, 
                        "\nCoefficients: disturbance with RE = ", re$distCoeff, ", expectedDensity with RE = ", re$expectedDensCoeff,
                        "\nCoefficients: disturbance without RE = ", noRE$distCoeff, ", expectedDensity without RE = ", noRE$expectedDensCoeff))
  
  return(list(plot = p, dt = DT))
}