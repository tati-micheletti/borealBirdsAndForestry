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

max(vif.mer(BBWAglmmi3)) #1.811573 NOT BAD!
max(vif.mer(BBWAglmmi4))

# About vif and the c. function check https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/.