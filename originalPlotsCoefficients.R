#### PLOTING MODEL COEFFICIENTS 
par(mfrow =c(1,2))

setwd("D:\\Human_disturbances\\Analyses\\Results\\BEAD single\\One visit per year")

# LOCAL SCALE - ALL DISTURBANCES
# All disturbances = rbind of permanent and transitional



mySimOut$models$localTransitional


x = COEF[,2] #Proportion of disturbance coefficient
species_names = COEF [,1] # Species name
c1 = COEF[,4] # lowerCI
c2 = COEF [,5] # upperCI
i = COEF [,6] # signifficance 

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Local scale effects", ylab= "Species", xlim=c(-2,0.5), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.2, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)