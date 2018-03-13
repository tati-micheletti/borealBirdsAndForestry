#--------------------- DATA TEST ------------------------#

models <- readRDS(file.path(getwd(),"modelsTrialforGraphs2.rds"))

mod1 <- models[[1]][[1]][[1]][[1]]
mod2 <- models[[1]][[2]][[1]][[1]]

mod3 <- models[[2]][[1]][[1]][[1]]
mod4 <- models[[2]][[2]][[1]][[1]]

localTransitional <- list(BBWA = mod1, CONW = mod2)
neighborhoodTransitional <- list(BBWA = mod3, CONW = mod4)

models <- list(localTransitional = localTransitional, neighborhoodTransitional = neighborhoodTransitional)

#----------------------------------------------------------

plotCoefficients <- function(dataset, combinations = sim$combinations, species = sim$birdSpecies){
  
  require(lmerTest)
  require(data.table)
  
  coefTable <- lapply(X = combinations, FUN = function(x){
    
    birdSpecies <- lapply(X = species, FUN = function(name){
      
      isolatedModel <- eval(parse(text = paste0("dataset[[x]]$",name)))
      coef <- as.data.frame(summary(isolatedModel)$coefficients, keep.rownames = TRUE)
      estimate <- coef["get(dimension)",][1:2]
      rownames(estimate) <- name
      estimate$lowerCI <- estimate$Estimate - estimate$`Std. Error`
      estimate$upperCI <- estimate$Estimate + estimate$`Std. Error`
      return(estimate)
    })

    birdSpeciesUnlisted <- do.call(rbind, unname(birdSpecies))

    return(birdSpeciesUnlisted)
    
  })
  
  names(coefTable) <- combinations
  
  plotList <- lapply(X = combinations, FUN = function(x){
    
    
    ## RETHINK HERE! To make a facet_grid, I need to use variables as All disturbances, 
    # Permanent and Transitional as Columns and Transitional, Permanent and Both as rows
    # The one below is probably not the best approach! Don't forget to add SIGNIFICANCY of MODELS 
    # as TRUE/FALSE!
    
  tempPlot <- ggplot() +
                      facet_grid()
      
      # plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Local scale effects", ylab= "Species", xlim=c(-2,0.5), yaxt="n", main="")
      # segments(c1, 1:length(x), c2, 1:length(x))
      # text(c1-0.2, 1:length(x), species_names, cex=0.9)
      # abline(h=0, v=0, lty=2)
  
      return(tempPlot)
  })
  
    browser()
    
  } )
}

plotCoefficients(dataset = models, 
                 combinations = c("localTransitional", "neighborhoodTransitional"), 
                 species = c("BBWA", "CONW"))