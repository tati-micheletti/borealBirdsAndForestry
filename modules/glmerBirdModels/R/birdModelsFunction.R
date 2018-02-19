# birdModels Function

birdModelsFunction <- function(data){
  
  # RESTART HERE! Re-write this code and make it better...
  
  require(lme4)
  
  ###### TRANSITIONAL DISTURBANCES
  DATA1 = read.csv("Final_points_BEAD.csv")
  # Local scale
  DATA2 = subset(DATA1, DATA1$State_P_100==0)
  DATA3 = subset (DATA1, DATA1$Agent_L=="Transitional")
  DATA=rbind(DATA2,DATA3)
  # Neighborhood scale
  DATA2 = subset(DATA1, DATA1$State_P_500==0)
  DATA3 = subset (DATA1, DATA1$Agent_N=="Transitional")
  DATA=rbind(DATA2,DATA3)
  # Neighborhood scale - local intact
  DATA2 = subset(DATA1, DATA1$State_P_100==0)
  DATA3 = subset (DATA2, DATA2$State_P_500==0)
  DATA4 = subset (DATA2, DATA2$Agent_N=="Transitional")
  DATA=rbind(DATA3,DATA4)
  
  ##### PERMANENT DISTURBANCES
  DATA1 = read.csv("Final_points_BEAD.csv")  
  # Local scale
  DATA2 = subset(DATA1, DATA1$State_P_100==0)
  DATA3 = subset (DATA1, DATA1$Agent_L=="Permanent")
  DATA=rbind(DATA2,DATA3)
  # Neighborhood scale
  DATA2 = subset(DATA1, DATA1$State_P_500==0)
  DATA3 = subset (DATA1, DATA1$Agent_L=="Permanent")
  DATA=rbind(DATA2,DATA3)
  # Neighborhood scale - local intact
  DATA2 = subset(DATA1, DATA1$State_P_100==0)
  DATA3 = subset (DATA2, DATA2$State_P_500==0)
  DATA4 = subset (DATA2, DATA2$Agent_N=="Permanent")
  DATA=rbind(DATA3,DATA4)
  
  
  ##### MODELS
  # LOCAL SCALE
  
  DATA = read.csv("Final_points_BEAD_perm_local.csv")
  DATA = read.csv("Final_points_BEAD_trans_local.csv")
  
  BBWAglmmi = glmer(AB_BBWA ~ State_P_100 + LOG_BCR_BBWA + offset(OF_BBWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  BLPWglmmi = glmer(AB_BLPW ~ State_P_100 + LOG_BCR_BLPW + offset(OF_BLPW) + (1|ClusterSP) + (1|YYYY), family="poisson", data=DATA)
  BOCHglmmi = glmer(AB_BOCH ~ State_P_100 + LOG_BCR_BOCH + offset(OF_BOCH) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  BRCRglmmi = glmer(AB_BRCR ~ State_P_100 + LOG_BCR_BRCR + offset(OF_BRCR) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  BTNWglmmi = glmer(AB_BTNW ~ State_P_100 + LOG_BCR_BTNW + offset(OF_BTNW) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  CAWAglmmi = glmer(AB_CAWA ~ State_P_100 + LOG_BCR_CAWA + offset(OF_CAWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  CMWAglmmi = glmer(AB_CMWA ~ State_P_100 + LOG_BCR_CMWA + offset(OF_CMWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  CONWglmmi = glmer(AB_CONW ~ State_P_100 + LOG_BCR_CONW + offset(OF_CONW) + (1|ClusterSP) + (1|YYYY), family="poisson", data=DATA)
  #FOSPglmmi = glmer(AB_FOSP ~ State_P_100 + LOG_BCR_FOSP + offset(OF_FOSP) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  #NOWAglmmi = glmer(AB_NOWA ~ State_P_100 + LOG_BCR_NOWA + offset(OF_NOWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  OVENglmmi = glmer(AB_OVEN ~ State_P_100 + LOG_BCR_OVEN + offset(OF_OVEN) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  PISIglmmi = glmer(AB_PISI ~ State_P_100 + LOG_BCR_PISI + offset(OF_PISI) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  RBNUglmmi = glmer(AB_RBNU ~ State_P_100 + LOG_BCR_RBNU + offset(OF_RBNU) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  SWTHglmmi = glmer(AB_SWTH ~ State_P_100 + LOG_BCR_SWTH + offset(OF_SWTH) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  TEWAglmmi = glmer(AB_TEWA ~ State_P_100 + LOG_BCR_TEWA + offset(OF_TEWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  WETAglmmi = glmer(AB_WETA ~ State_P_100 + LOG_BCR_WETA + offset(OF_WETA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  YRWAglmmi = glmer(AB_YRWA ~ State_P_100 + LOG_BCR_YRWA + offset(OF_YRWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  
  # NEIGHBORHOOD SCALE
  
  # FULL DATABASE
  DATA = read.csv("Final_points_BEAD.csv")
  DATA = read.csv("Final_points_BEAD_perm_neigh.csv")
  DATA = read.csv("Final_points_BEAD_trans_neigh.csv")
  
  # LOCAL DISTURBED
  DATA = read.csv("Final_points_BEAD_neigh_localdisturbed.csv")
  DATA = read.csv("Final_points_BEAD_perm_neigh_localdisturbed.csv")
  DATA = read.csv("Final_points_BEAD_trans_neigh_localdisturbed.CSV")
  
  # LOCAL INTACT
  DATA = read.csv("Final_points_BEAD_neigh_localintact.csv")
  DATA = read.csv("Final_points_BEAD_perm_neigh_localintact.csv")
  DATA = read.csv("Final_points_BEAD_trans_neigh_localintact.csv")
  
  BBWAglmmi2 = glmer(AB_BBWA ~ State_P_500 + LOG_BCR_BBWA + offset(OF_BBWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  BLPWglmmi2 = glmer(AB_BLPW ~ State_P_500 + LOG_BCR_BLPW + offset(OF_BLPW) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  BOCHglmmi2 = glmer(AB_BOCH ~ State_P_500 + LOG_BCR_BOCH + offset(OF_BOCH) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  BRCRglmmi2 = glmer(AB_BRCR ~ State_P_500 + LOG_BCR_BRCR + offset(OF_BRCR) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  BTNWglmmi2 = glmer(AB_BTNW ~ State_P_500 + LOG_BCR_BTNW + offset(OF_BTNW) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  CAWAglmmi2 = glmer(AB_CAWA ~ State_P_500 + LOG_BCR_CAWA + offset(OF_CAWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  CMWAglmmi2 = glmer(AB_CMWA ~ State_P_500 + LOG_BCR_CMWA + offset(OF_CMWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  CONWglmmi2 = glmer(AB_CONW ~ State_P_500 + LOG_BCR_CONW + offset(OF_CONW) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  FOSPglmmi2 = glmer(AB_FOSP ~ State_P_500 + LOG_BCR_FOSP + offset(OF_FOSP) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  NOWAglmmi2 = glmer(AB_NOWA ~ State_P_500 + LOG_BCR_NOWA + offset(OF_NOWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  OVENglmmi2 = glmer(AB_OVEN ~ State_P_500 + LOG_BCR_OVEN + offset(OF_OVEN) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  PISIglmmi2 = glmer(AB_PISI ~ State_P_500 + LOG_BCR_PISI + offset(OF_PISI) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  RBNUglmmi2 = glmer(AB_RBNU ~ State_P_500 + LOG_BCR_RBNU + offset(OF_RBNU) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  SWTHglmmi2 = glmer(AB_SWTH ~ State_P_500 + LOG_BCR_SWTH + offset(OF_SWTH) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  TEWAglmmi2 = glmer(AB_TEWA ~ State_P_500 + LOG_BCR_TEWA + offset(OF_TEWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  WETAglmmi2 = glmer(AB_WETA ~ State_P_500 + LOG_BCR_WETA + offset(OF_WETA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  YRWAglmmi2 = glmer(AB_YRWA ~ State_P_500 + LOG_BCR_YRWA + offset(OF_YRWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=DATA)
  
  return()
}