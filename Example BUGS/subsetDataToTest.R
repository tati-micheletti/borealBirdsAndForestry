library(data.table)
library(reproducible)
library(lme4)

wdir <- "C:/Users/tmichele/GitHub/borealBirdsAndForestry/Example BUGS"
dt <- data.table::fread(file.path(wdir, "Alb_DATA.csv")) # 34357 rows

# Cleaning the DT so I can run as a test data 

subAlb2008 <- dt[PROV == "AB" & 
                   YYYY %in% c(2007:2008) & 
                   (Agent_N == "Transitional" | Agent_N == "") & 
                   (Agent_L == "Transitional" | Agent_L == "")]

birdsToKeep <- c("TEWA", "RBNU", "OVEN", "PISI")
colsWithBirdData <- names(subAlb2008)[grepl(pattern = paste(birdsToKeep, collapse = "|"), x = names(subAlb2008))] %>%
  .[!grepl(x = ., pattern = "LOG")]
colsToKeep <- c("State_P_100", "State_P_500", "ClusterSP", "YYYY", colsWithBirdData)
dataForModel <- subAlb2008[, ..colsToKeep]

# Model Neighborhood: AB_sp ~ BCR_sp + State_P_100 + OFFSET(OF_sp) + RE (YYYY, ClusterSP, YYYY*ClusterSP)
# Model Local: AB_sp ~ BCR_sp + State_P_500 + OFFSET(OF_sp) + RE (YYYY, ClusterSP, YYYY*ClusterSP)

fitTEWA <- glmer(AB_TEWA ~ BCR_TEWA + State_P_100 + (1|YYYY) + (1|ClusterSP), offset = OF_TEWA, data = dataForModel, family = poisson)
sumTEWA <- summary(fitTEWA)

# Now use unmark and then NIMBLE

# ~~~~~ From here on

# # Load unmarked, format data in unmarked data frame and summarize
library(unmarked)
umf <- unmarkedFramePCount(
  y=C,                                            # Counts matrix
  siteCovs= data.frame(vegHt = vegHt, hab = hab), # Site covariates
  obsCovs = list(time = time, wind = wind))       # Observation covs
summary(umf)

