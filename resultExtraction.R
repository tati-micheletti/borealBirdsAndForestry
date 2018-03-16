# Investigating Results

# Effects of anthropogenic disturbances at local scales
plotList <- data.table(plotList)
negativeLocal <- plotList[Estimate < 0 & p < 0.05 & typeDisturbance == "LOCAL"]
uniqueSpecies <- unique(negativeLocal$Species)
length(uniqueSpecies)
negativeLocal[order(Species)]

# For the mean relative change
plotList.localBoth <- plotList[typeDisturbance=="LOCAL" & !disturbanceDimension=="BOTH"]
distProb <- data.frame(distProb = seq(from = 0, to = 1, by = 0.1))
plotList.localBoth <- plotList.localBoth[rep(seq_len(nrow(plotList.localBoth)), each=nrow(distProb)),]
expanded <- cbind(plotList.localBoth, distProb)
expanded$coeff <- exp(expanded$Estimate*expanded$distProb)
plTrans <- expanded[disturbanceDimension=="TRANSITIONAL"]
plPerm <- expanded[disturbanceDimension=="PERMANENT"]
meanRed.T <- plTrans[,c("Species","coeff")]
meanRed.P <- plPerm[,c("Species","coeff")]
require(reshape2)

# use apply
# NewCol : Calculating mean by species for both T and P
    # Extract 1 from each mean (on the same formula above!)
# NewCol :: reductions are calculated as value at (100% - 1) * 100
