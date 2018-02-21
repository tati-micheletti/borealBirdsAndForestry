# TO COMPARE:

BBWAglmmi = glmer(AB_BBWA ~ State_P_100 + LOG_BCR_BBWA + offset(OF_BBWA) + (1|ClusterSP) + 
                    (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=localTrans)
BBWAglmmi2 = glmer(AB_BBWA ~ State_P_500 + LOG_BCR_BBWA + offset(OF_BBWA) + (1|ClusterSP) +
                     (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=neighTrans)
BBWAglmmi3 = glmer(AB_BBWA ~ State_P_500 + State_P_100 + LOG_BCR_BBWA + offset(OF_BBWA) + (1|ClusterSP) +
                     (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=fullTable)

BBWAglmmi4 = glmer(AB_BBWA ~ c.(State_P_500) + c.(State_P_100) + LOG_BCR_BBWA + offset(OF_BBWA) + (1|ClusterSP) +
                     (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=fullTable)

summary(BBWAglmmi)
summary(BBWAglmmi2)
summary(BBWAglmmi3)
summary(BBWAglmmi4)

# aic <- AIC(BBWAglmmi, BBWAglmmi2, BBWAglmmi3)
anova.test <- anova(BBWAglmmi, BBWAglmmi2, BBWAglmmi3)

max(vif.mer(BBWAglmmi3)) #1.811573 NOT BAD!
max(vif.mer(BBWAglmmi4))

# TOMORROW: CHECK OUT HOW THE DENSITIES VARY THROUGH YEARS FOR EACH BIRD*BEC_LCC thing... 
# If they don't vary it is settled! If they vary, use the oldest one and hope for the best...