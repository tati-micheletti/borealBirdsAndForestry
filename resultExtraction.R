## Investigating Results
library(SpaDES)
require(reshape2)

mySimOut <- readRDS(file.path(getwd(), "outputs", "AlbertosModels14Mar18.rds"))
plotList <- plotList(dataset = mySimOut$models, 
                                 combinations = mySimOut$combinations, 
                                 species = mySimOut$birdSpecies)
plotList <- data.table(plotList)

#----------------------- LOCAL ---------------------#

# Effects of anthropogenic disturbances at local scales
negativeLocal <- plotList[Estimate < 0 & p < 0.05 & typeDisturbance == "LOCAL"]
uniqueSpecies <- unique(negativeLocal$Species)
length(uniqueSpecies)

# For the mean relative change
plotList.localBoth <- plotList[typeDisturbance=="LOCAL" & !disturbanceDimension=="BOTH"]
distProb <- data.frame(distProb = seq(from = 0, to = 1, by = 0.1))
plotList.localBoth <- plotList.localBoth[rep(seq_len(nrow(plotList.localBoth)), each=nrow(distProb)),]
expanded <- cbind(plotList.localBoth, distProb)
expanded$coeff <- exp(expanded$Estimate*expanded$distProb)
plTrans <- expanded[disturbanceDimension=="TRANSITIONAL"]
plPerm <- expanded[disturbanceDimension=="PERMANENT"]
densityReductionTrans <- plTrans[,c("Species","coeff","distProb")] %>%
      dcast(distProb ~ Species, value.var = "coeff")  %>%
        colMeans()-1 %>%
          min()
densityReductionTrans <- densityReductionTrans[densityReductionTrans<0] %>% .[-1]
densityReductionPerm <- plPerm[,c("Species","coeff","distProb")] %>%
      dcast(distProb ~ Species, value.var = "coeff") %>%
        colMeans()-1 %>%
          min()
densityReductionPerm <- densityReductionPerm[densityReductionPerm<0] %>% .[-1]

L.minMaxTrans <- round(c(min(densityReductionTrans),max(densityReductionTrans))*100, 0)
L.minMaxPerm <- round(c(min(densityReductionPerm),max(densityReductionPerm))*100, 0)

# Reductions up to...
reductTrans <- plTrans[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
   last()-1
L.reductTrans <- min(reductTrans)*100

reductPerm <- plPerm[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
   last()-1 
L.reductPerm <- min(reductPerm)*100

# Mean increase:
densityIncrePerm <- plPerm[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
  colMeans()-1
L.densityIncrePerm <- densityIncrePerm[densityIncrePerm>0]*100

densityIncreTrans <- plTrans[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff")  %>%
  colMeans()-1 
L.densityIncreTrans <- densityIncreTrans[densityIncreTrans>0]*100

# SUMMARY
L.minMaxTrans
L.minMaxPerm
L.reductTrans
L.reductPerm
L.densityIncrePerm
L.densityIncreTrans

#----------------------- NEIGHBORHOOD ---------------------#

# Effects of anthropogenic disturbances at local scales
negativeNeighborhood <- plotList[Estimate < 0 & p < 0.05 & typeDisturbance == "NEIGHBORHOOD"]
uniqueSpecies <- unique(negativeNeighborhood$Species)
length(uniqueSpecies)

# For the mean relative change
plotList.neighborhoodBoth <- plotList[typeDisturbance=="NEIGHBORHOOD" & !disturbanceDimension=="BOTH"]
distProb <- data.frame(distProb = seq(from = 0, to = 1, by = 0.1))
plotList.neighborhoodBoth <- plotList.neighborhoodBoth[rep(seq_len(nrow(plotList.neighborhoodBoth)), each=nrow(distProb)),]
expanded <- cbind(plotList.neighborhoodBoth, distProb)
expanded$coeff <- exp(expanded$Estimate*expanded$distProb)
plTrans <- expanded[disturbanceDimension=="TRANSITIONAL"]
plPerm <- expanded[disturbanceDimension=="PERMANENT"]
densityReductionTrans <- plTrans[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff")  %>%
  colMeans()-1 %>%
  min()
densityReductionTrans <- densityReductionTrans[densityReductionTrans<0] %>% .[-1]
densityReductionPerm <- plPerm[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
  colMeans()-1 %>%
  min()
densityReductionPerm <- densityReductionPerm[densityReductionPerm<0] %>% .[-1]

N.minMaxTrans <- round(c(min(densityReductionTrans),max(densityReductionTrans))*100, 0)
N.minMaxPerm <- round(c(min(densityReductionPerm),max(densityReductionPerm))*100, 0)

# Reductions up to...
reductTrans <- plTrans[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
  last()-1
N.reductTrans <- min(reductTrans)*100

reductPerm <- plPerm[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
  last()-1 
N.reductPerm <- min(reductPerm)*100

# Mean increase:
densityIncrePerm <- plPerm[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
  colMeans()-1
N.densityIncrePerm <- densityIncrePerm[densityIncrePerm>0]*100

densityIncreTrans <- plTrans[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff")  %>%
  colMeans()-1 
N.densityIncreTrans <- densityIncreTrans[densityIncreTrans>0]*100

# SUMMARY
N.minMaxTrans
N.minMaxPerm
N.reductTrans
N.reductPerm
N.densityIncrePerm
N.densityIncreTrans


#----------------------- LOCAL UNDISTURBED ---------------------#

# Effects of anthropogenic disturbances at local scales
negativeLU <- plotList[Estimate < 0 & p < 0.05 & typeDisturbance == "LOCAL UNDISTURBED"]
uniqueSpecies <- unique(negativeLU$Species)
length(uniqueSpecies)

# For the mean relative change
plotList.LUBoth <- plotList[typeDisturbance=="LOCAL UNDISTURBED" & !disturbanceDimension=="BOTH"]
distProb <- data.frame(distProb = seq(from = 0, to = 1, by = 0.1))
plotList.LUBoth <- plotList.LUBoth[rep(seq_len(nrow(plotList.LUBoth)), each=nrow(distProb)),]
expanded <- cbind(plotList.LUBoth, distProb)
expanded$coeff <- exp(expanded$Estimate*expanded$distProb)
plTrans <- expanded[disturbanceDimension=="TRANSITIONAL"]
plPerm <- expanded[disturbanceDimension=="PERMANENT"]
densityReductionTrans <- plTrans[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff")  %>%
  colMeans()-1 %>%
  min()
densityReductionTrans <- densityReductionTrans[densityReductionTrans<0] %>% .[-1]
densityReductionPerm <- plPerm[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
  colMeans()-1 %>%
  min()
densityReductionPerm <- densityReductionPerm[densityReductionPerm<0] %>% .[-1]

LU.minMaxTrans <- round(c(min(densityReductionTrans),max(densityReductionTrans))*100, 0)
LU.minMaxPerm <- round(c(min(densityReductionPerm),max(densityReductionPerm))*100, 0)

# Reductions up to...
reductTrans <- plTrans[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
  last()-1
LU.reductTrans <- min(reductTrans)*100

reductPerm <- plPerm[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
  last()-1 
LU.reductPerm <- min(reductPerm)*100

# Mean increase:
densityIncrePerm <- plPerm[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff") %>%
  colMeans()-1
LU.densityIncrePerm <- densityIncrePerm[densityIncrePerm>0]*100

densityIncreTrans <- plTrans[,c("Species","coeff","distProb")] %>%
  dcast(distProb ~ Species, value.var = "coeff")  %>%
  colMeans()-1 
LU.densityIncreTrans <- densityIncreTrans[densityIncreTrans>0]*100

# SUMMARY
LU.minMaxTrans
LU.minMaxPerm
LU.reductTrans
LU.reductPerm
LU.densityIncrePerm
LU.densityIncreTrans


# plotList
#plotList2 <- reshape2::dcast(plotList, Species ~ typeDisturbance + disturbanceDimension, value.var="Estimate") #,"Std.Error","p","lowerCI","upperCI","Significancy" # Need to see further how to do this better. Right now, it has to be as saved.
write.csv(plotList2, file.path(getwd(),"outputs","plotList.csv"))

