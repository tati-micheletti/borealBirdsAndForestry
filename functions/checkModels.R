checkModels <- function(useRE, species, dt, disturbancePerc, multiplyByExpectedDensity = FALSE){
  
  library("lme4")
  source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/createModel.R')
  # Create the model object from data (dt)
  if (useRE){
    m <- createModel(bird = species, data = dt, useRE = TRUE)
    message("Model class ", class(m), " created")
  } else {
    m <- createModel(bird = species, data = dt, useRE = FALSE)
    message("Model class ", class(m), " created")
  }
  
  if ("glmerMod" %in% class(m)){
    beta0 <- m@beta[1] # intercept
    beta1 <- m@beta[2] # Disturbance
    beta2 <- m@beta[3] # Expected Density
    name1 <- names(m@frame)[2] # Disturbance
    name2 <- names(m@frame)[3] # Expected Density
  } else {
    name0 <- names(m$coefficients)[1] # intercept
    name1 <- names(m$coefficients)[2] # Disturbance
    name2 <- names(m$coefficients)[3] # Expected Density
    beta0 <- m$coefficients[name0] # intercept
    beta1 <- m$coefficients[name1] # Disturbance
    beta2 <- m$coefficients[name2] # Expected Density
  }
  # EXPECTED DENSITY SOLYMOS & STRALBERG
  expectedDensity <- dt[[paste0("DENSITY_", species)]] # BLUE

  # PREDICTING WITH FORMULA
  newDT <- data.frame(disturbancePerc, log(expectedDensity))
  colnames(newDT) <- c(name1, name2)
  realPredicted <- fitModel(inRas = newDT,  # RED
                            inputModel = m, 
                            x = species,
                            tileYear = paste0("checking model"))
  multFactor <- expectedDensity
  if (!multiplyByExpectedDensity)
    multFactor <- 1
  summaryDT <- data.table::data.table(SuarezModelPredicted = unique(realPredicted*multFactor), 
                                       SolymosStralberg = unique(expectedDensity))
  setkey(summaryDT, SolymosStralberg)
  summaryDT$x <- 1:NROW(summaryDT)
  summaryDT <- melt(summaryDT, id.vars = "x")
  library("ggplot2")
  p <- ggplot(data = summaryDT, aes(x = x, y = value, col = variable)) +
    geom_line() +
    theme(legend.position = "bottom") +
    labs(title = paste0("Checking ", species, " for ", ifelse(isTRUE(useRE), 
                                                              " glmer ", " glm "), "model"))
  return(list(plot = p, dt = summaryDT, distCoeff = round(beta1, 4), expectedDensCoeff = round(beta2, 4)))
}