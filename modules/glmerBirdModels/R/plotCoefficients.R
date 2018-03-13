# Plots of model coefficients (Figures 3 and 4 from Alberto's manuscript)
# 
# mySimOut <- readRDS(file.path(getwd(),"modelsResults_11MARCH.rds"))
# 
# require(lmerTest)
# require(data.table)
# 
# coefTable <- as.data.table(summary(mySimOut$models$neighborhoodTransitional$BBWA)$coefficients)
# 
# 
