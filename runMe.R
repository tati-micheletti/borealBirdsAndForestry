# Global script for the Backcasting Project REMODELED and UPDATED

if (all(pemisc::user() %in% c("Tati", "tmichele"), 
        getwd() != "/home/tmichele/projects/borealBirdsAndForestry"))
  warning(paste0("Your current working directory is ", getwd(), 
                 ". Please make sure it is correct!"), 
          immediate. = TRUE)
source("1_generalSetup.R")
source("2_simulationSetup.R")