
# TO COMPARE ALBERTO's DATASET WITH THE SQL, INSERT THE CODE BELOW IN dataUploading.R, inside the glmerBirdModels.R

# ~~~~~~ OBS. ~~~~~~~~~~~~~~~~~~~~~~~~~
# === ALBERTO'S DATA: 
# "OF_species" (offsets)
# "BCR_species" (densities)
# "LOG_BCR_species" (log densities)
# "AB_species" (abundances)
# 
# === SQL AND PETER SOLYMOS' DATA:
# "OFFSET_species" (offsets)
# "DENSITY_species" (densities)
# "species" (abundances)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# =============== TO COMPARE ABUNDANCES ========================

browser()

# Subset from Alberto's table only what we will use from it:
colsKeepAlberto <- c("PKEY", "X", "Y", "YYYY", "State_P_100", "State_P_500", 
                     "Agent_100", "Agent_L", "Agent_500", "Agent_N", "ClusterSP",
                     "AB_BBWA", "AB_BTNW", "AB_CAWA", "AB_YRWA",
                     "BCR_BBWA", "BCR_BTNW", "BCR_CAWA", "BCR_YRWA",
                     "OF_BBWA", "OF_BTNW", "OF_CAWA", "OF_YRWA",
                     "DENSITY_BBWA", "DENSITY_BTNW", "DENSITY_CAWA", "DENSITY_YRWA",
                     "OFFSET_BBWA", "OFFSET_BTNW", "OFFSET_CAWA", "OFFSET_YRWA") # These can be excluded after comparing and IDing that we are using the SQL data
AlbertosData <- fullData[,..colsKeepAlberto]

# Merge Alberto's Data with original DB. Doing this as Alberto filtered his data for forested areas.
fullDataCheck <- merge(Count_XY_PKEY_cast_reduced, AlbertosData, by = "PKEY", all.x = FALSE, all.y = TRUE)

# Comparing the ABUNDANCE data (Data is not yet filtered for Cluster's 0 / NA # FOR STEVE

Alberto_BBWA <- fullDataCheck[, AB_BBWA]
SQL_BBWA <- fullDataCheck[, BBWA]
hist(Alberto_BBWA)
hist(SQL_BBWA)
diff_BBWA <- SQL_BBWA - Alberto_BBWA
min(diff_BBWA, na.rm = TRUE)
max(diff_BBWA, na.rm = TRUE)

Alberto_BTNW <- fullDataCheck[, AB_BTNW]
SQL_BTNW <- fullDataCheck[, BTNW]
hist(Alberto_BTNW)
hist(SQL_BTNW)
diff_BTNW <- SQL_BTNW - Alberto_BTNW
min(diff_BTNW, na.rm = TRUE)
max(diff_BTNW, na.rm = TRUE)

Alberto_CAWA <- fullDataCheck[, AB_CAWA]
SQL_CAWA <- fullDataCheck[, CAWA]
hist(Alberto_CAWA)
hist(SQL_CAWA)
diff_CAWA <- SQL_CAWA - Alberto_CAWA
min(diff_CAWA, na.rm = TRUE)
max(diff_CAWA, na.rm = TRUE)

Alberto_YRWA <- fullDataCheck[, AB_YRWA]
SQL_YRWA <- fullDataCheck[, YRWA]
hist(Alberto_YRWA)
hist(SQL_YRWA)
diff_YRWA <- SQL_YRWA - Alberto_YRWA
min(diff_YRWA, na.rm = TRUE)
max(diff_YRWA, na.rm = TRUE)


# Comparing the OFFSET data (Data is not yet filtered for Cluster's 0 / NA

Alberto_BBWA_OF <- fullDataCheck[, OF_BBWA]
SQL_BBWA_OF <- fullDataCheck[, OFFSET_BBWA]
hist(exp(Alberto_BBWA_OF))
hist(exp(SQL_BBWA_OF))
diff_BBWA_OF <- SQL_BBWA_OF - Alberto_BBWA_OF
min(diff_BBWA_OF, na.rm = TRUE)
max(diff_BBWA_OF, na.rm = TRUE)

Alberto_BTNW_OF <- fullDataCheck[, OF_BTNW]
SQL_BTNW_OF <- fullDataCheck[, OFFSET_BTNW]
hist(exp(Alberto_BTNW_OF))
hist(exp(SQL_BTNW_OF))
diff_BTNW_OF <- SQL_BTNW_OF - Alberto_BTNW_OF
min(diff_BTNW_OF, na.rm = TRUE)
max(diff_BTNW_OF, na.rm = TRUE)

Alberto_CAWA_OF <- fullDataCheck[, OF_CAWA]
SQL_CAWA_OF <- fullDataCheck[, OFFSET_CAWA]
hist(exp(Alberto_CAWA_OF))
hist(exp(SQL_CAWA_OF))
diff_CAWA_OF <- SQL_CAWA_OF - Alberto_CAWA_OF
min(diff_CAWA_OF, na.rm = TRUE)
max(diff_CAWA_OF, na.rm = TRUE)

Alberto_YRWA_OF <- fullDataCheck[, OF_YRWA]
SQL_YRWA_OF <- fullDataCheck[, OFFSET_YRWA]
hist(exp(Alberto_YRWA_OF))
hist(exp(SQL_YRWA_OF))
diff_YRWA_OF <- SQL_YRWA_OF - Alberto_YRWA_OF
min(diff_YRWA_OF, na.rm = TRUE)
max(diff_YRWA_OF, na.rm = TRUE)


# Comparing the DENSITY data (Data is not yet filtered for Cluster's 0 / NA

Alberto_BBWA_BCR <- fullDataCheck[, BCR_BBWA]
SQL_BBWA_BCR <- fullDataCheck[, DENSITY_BBWA]
hist(Alberto_BBWA_BCR)
hist(SQL_BBWA_BCR)
diff_BBWA_BCR <- SQL_BBWA_BCR - Alberto_BBWA_BCR
min(diff_BBWA_BCR, na.rm = TRUE)
max(diff_BBWA_BCR, na.rm = TRUE)

Alberto_BTNW_BCR <- fullDataCheck[, BCR_BTNW]
SQL_BTNW_BCR <- fullDataCheck[, DENSITY_BTNW]
hist(Alberto_BTNW_BCR)
hist(SQL_BTNW_BCR)
diff_BTNW_BCR <- SQL_BTNW_BCR - Alberto_BTNW_BCR
min(diff_BTNW_BCR, na.rm = TRUE)
max(diff_BTNW_BCR, na.rm = TRUE)

Alberto_CAWA_BCR <- fullDataCheck[, BCR_CAWA]
SQL_CAWA_BCR <- fullDataCheck[, DENSITY_CAWA]
hist(Alberto_CAWA_BCR)
hist(SQL_CAWA_BCR)
diff_CAWA_BCR <- SQL_CAWA_BCR - Alberto_CAWA_BCR
min(diff_CAWA_BCR, na.rm = TRUE)
max(diff_CAWA_BCR, na.rm = TRUE)

Alberto_YRWA_BCR <- fullDataCheck[, BCR_YRWA]
SQL_YRWA_BCR <- fullDataCheck[, DENSITY_YRWA]
hist(Alberto_YRWA_BCR)
hist(SQL_YRWA_BCR)
diff_YRWA_BCR <- SQL_YRWA_BCR - Alberto_YRWA_BCR
min(diff_YRWA_BCR, na.rm = TRUE)
max(diff_YRWA_BCR, na.rm = TRUE)

# ======================= FINAL CHECK ABUNDANCE
