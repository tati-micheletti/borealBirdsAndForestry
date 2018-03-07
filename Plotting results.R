######### PLOTING RESULTS

setwd("D:\\Human_disturbances\\Analyses")
install.packages("ggplot2")
library(ggplot2)

#### Distribution of area disturbed per sector

  # LOCAL SCALE
DATA1 = read.csv("Final_points_BEAD.csv")
DATA2 = subset(DATA1, DATA1$State_P_100>0)
Disturbed_100 = subset (DATA2, DATA2$Agent_L=="Transitional")
Disturbed_100 = subset (DATA2, DATA2$Agent_L=="Permanent")

Legend=factor(Disturbed_100$Agent_100)
pl=qplot(Disturbed_100$State_P_100, geom="histogram", fill=Legend, xlab="Proportion of disturbed area, local scale", ylab="Number of surveys") 
pl+theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"),legend.text=element_text(size=12), legend.title=element_text(size=14))

  # NEIGHBORHOOD SCALE
DATA1 = read.csv("Final_points_BEAD.csv")
DATA2 = subset(DATA1, DATA1$State_P_500>0)
Disturbed_500 = subset (DATA2, DATA2$Agent_N=="Transitional")
Disturbed_500 = subset (DATA2, DATA2$Agent_N=="Permanent")

Legend=factor(Disturbed_500$Agent_500)
pn=qplot(Disturbed_500$State_P_500, geom="histogram", fill=Legend, xlab="Proportion of disturbed area, neighborhood scale", xlim=c(0,1), ylab="Number of surveys")
pn+theme(axis.text=element_text(size=12), axis.title=element_text(size=16,face="bold"),legend.text=element_text(size=12), legend.title=element_text(size=14))



#### PLOTING MODEL COEFFICIENTS 
par(mfrow =c(1,2))

setwd("D:\\Human_disturbances\\Analyses\\Results\\BEAD single\\One visit per year")

# LOCAL SCALE - ALL DISTURBANCES
COEF = read.csv("State_P_100_agent_all.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Local scale effects", ylab= "Species", xlim=c(-2,0.5), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.2, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)



# LOCAL SCALE - PERMANENT DISTURBANCES
COEF = read.csv("State_P_100_agent_perm.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Local scale effects", ylab= "Species", xlim=c(-2.55,0.8), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.2, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# LOCAL SCALE - TRANSITIONAL DISTURBANCES
COEF = read.csv("State_P_100_agent_trans.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Local scale effects", ylab= "Species", xlim=c(-1.9,0.3), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.2, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# NEIGHBORHOOD SCALE - ALL DISTURBANCES
COEF = read.csv("State_P_500_agent_all.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-2,1.2), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.3, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# NEIGHBORHOOD SCALE - PERMANENT DISTURBANCES
COEF = read.csv("State_P_500_agent_perm.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-3.2,1.6), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.5, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# NEIGHBORHOOD SCALE - TRANSITIONAL DISTURBANCES
COEF = read.csv("State_P_500_agent_trans.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-2.1,0.9), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.2, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# NEIGHBORHOOD SCALE - ALL DISTURBANCES - LOCAL INTACT
COEF = read.csv("State_P_500_agent_all_localintact.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-2.9,2.3), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.4, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# NEIGHBORHOOD SCALE - PERMANENT DISTURBANCES - LOCAL INTACT
COEF = read.csv("State_P_500_agent_perm_localintact.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-6.9,1.8), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.7, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# NEIGHBORHOOD SCALE - TRANSITIONAL DISTURBANCES - LOCAL INTACT
COEF = read.csv("State_P_500_agent_trans_localintact.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-3.6,2), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.4, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


######### PLOTING DISTRIBUTION OF OBSERVATIONS ACROSS YEARS AND CLUSTERS 

###### ALL DISTURBANCES
DATA1 = read.csv("Final_points_BEAD.csv")


###### LOCAL SCALE
    # PERMANENT DISTURBANCES
DATA = read.csv("Final_points_BEAD.csv")
DATA2 = subset (DATA, DATA$Agent_L=="Permanent")
DATA3 = subset (DATA, DATA$State_100==0)
DATA1 = rbind(DATA2,DATA3)

    # TRANSITIONAL DISTURBANCES
DATA = read.csv("Final_points_BEAD.csv")
DATA2 = subset (DATA, DATA$Agent_L=="Transitional")
DATA3 = subset (DATA, DATA$State_100==0)
DATA1 = rbind(DATA2,DATA3)

###### NEIGHBORHOOD SCALE
    # PERMANENT DISTURBANCES
DATA = read.csv("Final_points_BEAD.csv")
DATA2 = subset (DATA, DATA$Agent_N=="Permanent")
DATA3 = subset (DATA, DATA$State_500==0)
DATA1 = rbind(DATA2,DATA3)

    # TRANSITIONAL DISTURBANCES
DATA = read.csv("Final_points_BEAD.csv")
DATA2 = subset (DATA, DATA$Agent_N=="Transitional")
DATA3 = subset (DATA, DATA$State_500==0)
DATA1 = rbind(DATA2,DATA3)

  ## LOCAL INTACT
    # PERMANENT DISTURBANCES
DATA2 = read.csv("Final_points_BEAD.csv")
DATA = subset(DATA2, DATA2$State_P_100==0)
DATA3 = subset (DATA, DATA$Agent_N=="Permanent")
DATA4 = subset (DATA, DATA$State_500==0)
DATA1 = rbind(DATA3,DATA4)

    # TRANSITIONAL DISTURBANCES
DATA2 = read.csv("Final_points_BEAD.csv")
DATA = subset(DATA2, DATA2$State_P_100==0)
DATA3 = subset (DATA, DATA$Agent_N=="Transitional")
DATA4 = subset (DATA, DATA$State_500==0)
DATA1 = rbind(DATA3,DATA4)


#### PLOTS

plot(DATA1$AB_BBWA~DATA1$ClusterSP) 
plot(DATA1$AB_BBWA~DATA1$YYYY)     
DATA2 = subset(DATA1, DATA1$AB_BBWA>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_BLPW~DATA1$ClusterSP) 
plot(DATA1$AB_BLPW~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_BLPW>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_BOCH~DATA1$ClusterSP) 
plot(DATA1$AB_BOCH~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_BOCH>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_BRCR~DATA1$ClusterSP) 
plot(DATA1$AB_BRCR~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_BRCR>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_BTNW~DATA1$ClusterSP) 
plot(DATA1$AB_BTNW~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_BTNW>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_CAWA~DATA1$ClusterSP) 
plot(DATA1$AB_CAWA~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_CAWA>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_CMWA~DATA1$ClusterSP) 
plot(DATA1$AB_CMWA~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_CMWA>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_CONW~DATA1$ClusterSP) 
plot(DATA1$AB_CONW~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_CONW>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_FOSP~DATA1$ClusterSP) 
plot(DATA1$AB_FOSP~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_FOSP>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_NOWA~DATA1$ClusterSP) 
plot(DATA1$AB_NOWA~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_NOWA>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_OVEN~DATA1$ClusterSP) 
plot(DATA1$AB_OVEN~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_OVEN>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_PISI~DATA1$ClusterSP) 
plot(DATA1$AB_PISI~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_PISI>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_RBNU~DATA1$ClusterSP) 
plot(DATA1$AB_RBNU~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_RBNU>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_SWTH~DATA1$ClusterSP) 
plot(DATA1$AB_SWTH~DATA1$YYYY)     
DATA2 = subset(DATA1, DATA1$AB_SWTH>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_TEWA~DATA1$ClusterSP) 
plot(DATA1$AB_TEWA~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_TEWA>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_WETA~DATA1$ClusterSP)  
plot(DATA1$AB_WETA~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_WETA>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    

plot(DATA1$AB_YRWA~DATA1$ClusterSP) 
plot(DATA1$AB_YRWA~DATA1$YYYY)      
DATA2 = subset(DATA1, DATA1$AB_YRWA>0)
plot(DATA2$ClusterSP~DATA2$YYYY)    































############################# OLD STUFF ################################

#### KERNEL DENSITY ESTIMATION PLOTS

setwd("D:\\Human_disturbances\\Analyses")
DATA1 = read.csv("Final_points_BEAD.csv")
DATA1= subset (DATA2, DATA2$State_P_100==0)

Disturbed_100 = subset (DATA1, DATA1$State_P_100>0)
hist100=hist(Disturbed_100, plot=F)
median(Disturbed_100)
plot(density(Disturbed_100), main="Kernel Density Plot: State_P_100", xlab="State_P_100")

Disturbed_500 = subset (DATA1, DATA1$State_P_500>0)
hist(Disturbed_500$State_P_500, xlab="Proportion of area disturbed, neighborhood scale", ylab="Count", main="")
plot(density(Disturbed_500), main="Kernel Density Plot: State_P_500", xlab="State_P_500")

Disturbed_T = subset (State_P, State_P>0)
summary(Disturbed_T)
hist(Disturbed_T)
plot(density(Disturbed_T), main="Kernel Density Plot: State_P", xlab="State_P")

# HEXBIN PLOT

install.packages("hexbin")
library(hexbin)
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
setwd("D:\\Human_disturbances\\Analyses")
DATA = read.csv("Final_points_BEADs.csv")
hexbinplot(State_P_100~State_P_500, data=DATA, colramp=rf, trans=log, inv=exp)
summary(DATA)
DATA$State_P_500

# CUMULATIVE DISTRIBUTION FUNCTIONS

setwd("D:\\Human_disturbances\\Analyses")
DATA = read.csv("Final_points_reclass.csv")

Disturbed_100 = subset (DATA$State_P_100, DATA$State_P_100>0)
hist(Disturbed_100)
State_P_100.ecdf = ecdf(Disturbed_100)
plot(State_P_100.ecdf, main="Cumulative distribution function State_P_100", xlab="State_P_100", ylab="Cumulative probability")

Disturbed_500 = subset (DATA$State_P_500, DATA$State_P_500>0)
hist(Disturbed_500)
State_P_500.ecdf = ecdf(Disturbed_500)
plot(State_P_500.ecdf, main="Cumulative distribution function State_P_500", xlab="State_P_500", ylab="Cumulative probability")

##### MODEL COEFFICIENTS

# STATE P 100 temporary disturbances
COEF = read.csv("State_P_100_temp.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Local scale effects", ylab= "Species", xlim=c(-2.4,0.6), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.3, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# STATE P 100 permanent disturbances
COEF = read.csv("State_P_100_perm.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Local scale effects", ylab= "Species", xlim=c(-4.6,2.2), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.5, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# STATE P 500 full dataset
COEF = read.csv("State_P_500.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-1.9,1.2), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.3, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# STATE P 500 temporary disturbances
COEF = read.csv("State_P_500_temp.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-3.3,0.8), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.4, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# STATE P 500 permanent disturbances
COEF = read.csv("State_P_500_perm.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-9.8,4.5), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-1.1, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# STATE P 500 full dataset, local intact
COEF = read.csv("State_P_500_local_intact.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects", ylab= "Species", xlim=c(-7.9,2.6), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.8, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


####################### OLD (GFC+BEAD) ##############################


# STATE P 500 with State_100=0
COEF = read.csv("State_P_500xState_100=0.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects, local undisturbed", ylab= "Species", xlim=c(-3.2,1.2), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c1-0.3, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# STATE P 500 with State_100=1
COEF = read.csv("State_P_500xState_100=1.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects, local disturbed", ylab= "Species", xlim=c(-1.35,0.7), yaxt="n", main="")
segments(c1, 1:length(x), c2, 1:length(x))
text(c2-0.2, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# STATE P 500 with State_100_T=0
COEF = read.csv("State_P_500xState_100_T=0.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects, local undisturbed", ylab= "Species", xlim=c(-1.5,0.4), yaxt="n", main="Truncated")
segments(c1, 1:length(x), c2, 1:length(x))
text(c2-0.15, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)


# STATE P 500 with State_100_T=1
COEF = read.csv("State_P_500xState_100_T=1.csv")
#View(COEF)

x = COEF[,2]
species_names = COEF [,1]
c1 = COEF[,4]
c2 = COEF [,5]
i = COEF [,6]

plot(x, 1:length(x), pch=ifelse(i==1, 19, ifelse(i==2, 8, 21)), xlab="Neighborhood scale effects, local disturbed", ylab= "Species", xlim=c(-4.4,3.8), yaxt="n", main="Truncated")
segments(c1, 1:length(x), c2, 1:length(x))
text(c2-0.6, 1:length(x), species_names, cex=0.9)
abline(h=0, v=0, lty=2)




################### PLOTING RELATIONSHIP STATE_P_100 ~ STATE_P_500 ####################################

COEF = read.csv("100 VS 500 alone.csv")
State_P_100 = COEF[,2]
State_P_500 = COEF[,3]
species_names = COEF [,1]

plot(State_P_100, State_P_500, xlab="Local", ylab="Neighborhood", xlim=c(min(State_P_100)-0.1,max(State_P_100)+0.1), main="Separate models")
ANOVA = lm(State_P_500~State_P_100)
summary(ANOVA)
abline(ANOVA)
text(State_P_100+0.06, State_P_500, species_names, cex=0.9)
spearman=round(cor(State_P_100, State_P_500, use="complete.obs", method="spearman"),digits=5)
text(min(State_P_100),max(State_P_500)-0.05,"Spearman =", cex=0.95)
text(min(State_P_100)+0.138,max(State_P_500)-0.048,spearman, cex=0.84)


COEF = read.csv("100 VS 500 sum.csv")
State_P_100 = COEF[,2]
State_P_500 = COEF[,3]
species_names = COEF [,1]

plot(State_P_100, State_P_500, xlab="Local", ylab="Neighborhood", xlim=c(min(State_P_100)-0.1,max(State_P_100)+0.1), main="Additive models")
ANOVA = lm(State_P_500~State_P_100)
summary(ANOVA)
abline(ANOVA)
text(State_P_100+0.06, State_P_500, species_names, cex=0.9)
spearman=round(cor(State_P_100, State_P_500, use="complete.obs", method="spearman"),digits=5)
text(max(State_P_100-0.2),max(State_P_500)-0.05,"Spearman =", cex=0.95)
text(max(State_P_100-0.15)+0.138,max(State_P_500)-0.048,spearman, cex=0.84)


COEF = read.csv("100 VS 500 interac.csv")
State_P_100 = COEF[,2]
State_P_500 = COEF[,3]
species_names = COEF [,1]

plot(State_P_100, State_P_500, xlab="Local", ylab="Neighborhood", xlim=c(min(State_P_100)-0.1,max(State_P_100)+0.1), main="Interaction models")
ANOVA = lm(State_P_500~State_P_100)
summary(ANOVA)
abline(ANOVA)
text(State_P_100+0.06, State_P_500, species_names, cex=0.9)
spearman=round(cor(State_P_100, State_P_500, use="complete.obs", method="spearman"),digits=5)
text(min(State_P_100+0.1),max(State_P_500)-0.05,"Spearman =", cex=0.95)
text(min(State_P_100)+0.51,max(State_P_500)-0.048,spearman, cex=0.84)


#################### PLOTING RELATIONSHIP GLMM ~ GLM ################################

# STATE P 100 ALONE
par(mfrow =c(1,1))

COEF = read.csv("GLMMvsGLM State_100.csv")
GLMM = COEF[,2]
GLM = COEF[,3]
species_names = COEF [,1]

plot(GLMM, GLM, xlab="GLMM Coefficients", ylab="GLM Coefficients", xlim=c(min(GLMM)-0.1,max(GLMM)+0.1), main="State_P_100 GLMM vs GLM")
ANOVA = lm(GLM~GLMM)
summary(ANOVA)
abline(ANOVA)
text(GLMM+0.06, GLM, species_names, cex=0.9)
spearman=round(cor(GLMM, GLM, use="complete.obs", method="spearman"),digits=5)
text(min(GLMM),max(GLM)-0.05,"Spearman =", cex=0.95)
text(min(GLMM)+0.138,max(GLM)-0.048,spearman, cex=0.84)


# STATE P 500 ALONE
COEF = read.csv("GLMMvsGLM State_500.csv")
GLMM = COEF[,2]
GLM = COEF[,3]
species_names = COEF [,1]

plot(GLMM, GLM, xlab="GLMM Coefficients", ylab="GLM Coefficients", xlim=c(min(GLMM)-0.1,max(GLMM)+0.1), main="State_P_500 GLMM vs GLM")
ANOVA = lm(GLM~GLMM)
summary(ANOVA)
abline(ANOVA)
text(GLMM+0.09, GLM, species_names, cex=0.6)
spearman=round(cor(GLMM, GLM, use="complete.obs", method="spearman"),digits=5)
text(min(GLMM),max(GLM)-0.05,"Spearman =", cex=0.95)
text(min(GLMM)+0.17,max(GLM)-0.048,spearman, cex=0.84)


# STATE P 100 + STATE P 500 

par(mfrow =c(1,1))

# STATE P 100
COEF = read.csv("GLMMvsGLM State_100+500_100.csv")
GLMM = COEF[,2]
GLM = COEF[,3]
species_names = COEF [,1]

plot(GLMM, GLM, xlab="GLMM Coefficients State_100", ylab="GLM Coefficients State_100", xlim=c(min(GLMM)-0.1,max(GLMM)+0.1))
ANOVA = lm(GLM~GLMM)
summary(ANOVA)
abline(ANOVA)
text(GLMM+0.1, GLM, species_names, cex=0.45)
spearman=round(cor(GLMM, GLM, use="complete.obs", method="spearman"),digits=5)
text(min(GLMM)+0.15,max(GLM)-0.05,"Spearman =", cex=0.95)
text(min(GLMM)+0.59,max(GLM)-0.048,spearman, cex=0.84)

# STATE P 500
COEF = read.csv("GLMMvsGLM State_100+500_500.csv")
GLMM = COEF[,2]
GLM = COEF[,3]
species_names = COEF [,1]

plot(GLMM, GLM, xlab="GLMM Coefficients State_500", ylab="GLM Coefficients State_500", xlim=c(min(GLMM)-0.1,max(GLMM)+0.17))
ANOVA = lm(GLM~GLMM)
summary(ANOVA)
abline(ANOVA)
text(GLMM+0.15, GLM, species_names, cex=0.45)
spearman=round(cor(GLMM, GLM, use="complete.obs", method="spearman"),digits=5)
text(min(GLMM)+0.2,max(GLM)-0.05,"Spearman =", cex=0.9)
text(min(GLMM)+0.73,max(GLM)-0.047,spearman, cex=0.79)

# STATE P 100 * STATE P 500 

# STATE P 100
COEF = read.csv("GLMMvsGLM State 100x500_100.csv")
GLMM = COEF[,2]
GLM = COEF[,3]
species_names = COEF [,1]

plot(GLMM, GLM, xlab="GLMM Coefficients State_100", ylab="GLM Coefficients State_100", xlim=c(min(GLMM)-0.1,max(GLMM)+0.1))
ANOVA = lm(GLM~GLMM)
summary(ANOVA)
abline(ANOVA)
text(GLMM+0.2, GLM, species_names, cex=1)
spearman=round(cor(GLMM, GLM, use="complete.obs", method="spearman"),digits=5)
text(min(GLMM)+0.15,max(GLM)-0.05,"Spearman =", cex=0.95)
text(min(GLMM)+0.59,max(GLM)-0.048,spearman, cex=0.84)

# STATE P 500
COEF = read.csv("GLMMvsGLM State 100x500_500.csv")
GLMM = COEF[,2]
GLM = COEF[,3]
species_names = COEF [,1]

plot(GLMM, GLM, xlab="GLMM Coefficients State_500", ylab="GLM Coefficients State_500", xlim=c(min(GLMM)-0.1,max(GLMM)+0.17))
ANOVA = lm(GLM~GLMM)
summary(ANOVA)
abline(ANOVA)
text(GLMM+0.2, GLM, species_names, cex=1)
spearman=round(cor(GLMM, GLM, use="complete.obs", method="spearman"),digits=5)
text(min(GLMM)+0.1,max(GLM)-0.05,"Spearman =", cex=0.9)
text(min(GLMM)+0.42,max(GLM)-0.047,spearman, cex=0.79)

# STATE P 100 * STATE P 500
COEF = read.csv("GLMMvsGLM State 100x500_Int.csv")
GLMM = COEF[,2]
GLM = COEF[,3]
species_names = COEF [,1]

plot(GLMM, GLM, xlab="GLMM Coefficients State_100*State_500", ylab="GLM Coefficients State_100*State_500", xlim=c(min(GLMM)-0.1,max(GLMM)+0.17))
ANOVA = lm(GLM~GLMM)
summary(ANOVA)
abline(ANOVA)
text(GLMM+0.2, GLM, species_names, cex=0.8)
spearman=round(cor(GLMM, GLM, use="complete.obs", method="spearman"),digits=5)
text(min(GLMM)+0.2,max(GLM)-0.05,"Spearman =", cex=0.9)
text(min(GLMM)+0.95,max(GLM)-0.04,spearman, cex=0.79)



##### STATE P
COEF = read.csv("GLMMvsGLM State_P.csv")
GLMM = COEF[,2]
GLM = COEF[,3]
species_names = COEF [,1]

plot(GLMM, GLM, xlab="GLMM Coefficients State_P", ylab="GLM Coefficients State_P", xlim=c(min(GLMM)-0.1,max(GLMM)+0.17))
ANOVA = lm(GLM~GLMM)
summary(ANOVA)
abline(ANOVA)
text(GLMM+0.08, GLM, species_names, cex=0.6)
spearman=round(cor(GLMM, GLM, use="complete.obs", method="spearman"),digits=5)
text(min(GLMM),max(GLM)-0.05,"Spearman =", cex=0.9)
text(min(GLMM)+0.178,max(GLM)-0.047,spearman, cex=0.79)
