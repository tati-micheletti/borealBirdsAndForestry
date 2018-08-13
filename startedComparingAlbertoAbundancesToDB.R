# Testing Alberto's data on the side for concordance with DB in relation to abundance

keep <- names(fullDataset)[grep(x = names(fullDataset), pattern = "AB_")]
colsKeepAlberto <- c("PKEY", "X", "Y", "YYYY", "State_P_100", "State_P_500", "Agent_100", "Agent_L", "Agent_500", "Agent_N", "ClusterSP", keep)
AlbertosData <- fullDataset[,..colsKeepAlberto] 

# Right now we don't need info on JULIAN, DURATION and DISTANCE, etc because we are not calculating offsets. 
# But we can change it here if we want to do this on the fly.

# Exclude columns of species we are not using at the moment:
colsToKeep <- c("PKEY", "YYYY", "X", "Y", 
                "BBWA", "BLPW", "BOCH", "BRCR", 
                "BTNW", "CAWA", "CMWA", "CONW", 
                "OVEN", "PISI", "RBNU", "SWTH", 
                "TEWA", "WETA", "YRWA")

Count_XY_PKEY_cast_reduced <- Count_XY_PKEY_cast[, ..colsToKeep]
# Merge Alberto's Data with original DB. Doing this as Alberto filtered his data for some reason. The original DB has 
fullData <- merge(Count_XY_PKEY_cast_reduced, AlbertosData, by = "PKEY", all.x = FALSE, all.y = TRUE)

colsToKeepComp <- names(fullData)[c(5:19, 30:46)]
toCompare <- fullData[, ..colsToKeepComp]

BBWAcols <- grep(pattern = "BBWA", names(toCompare))
BBWA <- toCompare[, ..BBWAcols]
BBWA$comp <- BBWA$BBWA == BBWA$AB_BBWA
rows <- which(BBWA[,!is.na(comp)])
BBWA2 <- BBWA[rows]
BBWA2$consistency <- BBWA2$BBWA - BBWA2$AB_BBWA # DB - Alberto's
min(BBWA2$consistency)
max(BBWA2$consistency)
# Alberto's DS has always less abundance than the DB
# I found a file that says (Process of selecting points (with ArcGIS steps.docx)):
  # 3.Replace  NA  by  0  within  the  abundance  data  
  # 4.For  each  species,  identified  clusters  with  no  presence  (AB  =  0)  
    # a.Removed  the  abundance  data  for  those  clusters  (i.e.  replaced  0  by  NA)

Wrong <- length(which(BBWA2$comp == FALSE))
Right <- length(which(BBWA2$comp == TRUE))
ratio <- 100*(Wrong/(Wrong+Right))

# Alberto's data is not matching the 
