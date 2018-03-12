# Plots of model coefficients (Figures 3 and 4 from Alberto's manuscript)

require(lmerTest)

coefTable <- as.data.table(summary(mySimOut$models$neighborhoodTransitional$CONW)$coefficients)
