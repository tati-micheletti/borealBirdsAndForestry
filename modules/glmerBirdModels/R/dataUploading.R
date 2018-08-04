# dataUploading

# On 25JUL18 I saved the file as I was trying to start reproducing Alberto's data cleaning. We gave up, as I should be focusing on publishing. 
# I will now go back to a previous version of dataUploading() that was working, and only compare Alberto's file Final_points_2010.csv (nrow = 27874, ncol = 102, size 23,225kb)
# to the same file with updated densities (from Habitat_Association_by_jurisdiction(bamddb_Apr19-2012). Alberto used on his DDB_BCR_PROV_LCC_Apr4-2012.csv)

dataUploading <- function(dataset = sim$dataName, 
                          combinations = sim$combinations,
                          birdDensityRasters = sim$birdDensityRasters,
                          SQLTableName = "SQLData",
                          envirSim = envir(sim)){
  
  require(data.table)
  require(googledrive)
  
  data.path <- file.path(getwd(), "modules/glmerBirdModels/data", dataset)
  
  # Get Alberto's data
  if (file.exists(data.path)){
    fullDataset <- suppressWarnings(fread(data.path))}
  else {
    invisible(readline(prompt=paste("Make dure you have the dataset in Google Drives folder 'BAM/Datasets/borealBirdsAndForestry', and press [enter] to continue",
                                    "\nIf authentication fails, please manually place the dataset file in the folder: \n",
                                    file.path(getwd(), "modules/glmerBirdModels/data"))))
    require(googledrive)
    drive_download(file.path("BAM/Datasets/borealBirdsAndForestry", dataset), path = file.path(getwd(), "modules/glmerBirdModels/data", dataset), overwrite = TRUE,verbose = FALSE)
    fullDataset <- suppressWarnings(fread(file.path(getwd(), "modules/glmerBirdModels/data", dataset)))
  }
  
  # Subset from Alberto's table only what we will use from it:
  colsKeepAlberto <- c("PKEY", "X", "Y", "YYYY", "State_P_100", "State_P_500", "Agent_100", "Agent_L", "Agent_500", "Agent_N", "ClusterSP")
  AlbertosData <- fullDataset[,..colsKeepAlberto] # Alberto's dataset has "data" that doesn't exist in the database.
  # These are the PKEY's that are in Alberto's DS but not in the DB: 
  # c("RP:67-19-W5-OD:10:03:1", "RP:67-19-W5-OD:1:03:1", "RP:67-19-W5-OD:2:03:1", 
  #   "RP:67-19-W5-OD:3:03:1", "RP:67-19-W5-OD:4:03:1", "RP:67-19-W5-OD:5:03:1", 
  #   "RP:67-19-W5-OD:6:03:1", "RP:67-19-W5-OD:7:03:1", "RP:67-19-W5-OD:8:03:1", 
  #   "RP:67-19-W5-OD:9:03:1")
  
  browser()
  # Retrieve the SQL data
  Count_XY_PKEY_orig <- get(SQLTableName, envir = envirSim)
  
  # Need to filter to exclude ONE ficticious speciesthat was called "YYYY": 
  #          PKEY         SS    PCODE  SPECIES ABUND DURATION DISTANCE BEH   X       Y      METHOD   ROUND YYYY JULIAN SITE STN
  #  1: JNP:11:8:10:1 JNP:11:8   JNP    YYYY     1       16        7   1 -118.2602 52.74181 JNP:114     1  2010      0   11   8
  Count_XY_PKEY <- Count_XY_PKEY_orig[!SPECIES == "YYYY", ]
  
  # CAWA (among others, but we don't use other species) has a typo. CAWA's is in PKEY "MANBBA:14NG49:508896:1:11". Correct this one to 6 as per Alberto's suggestion.
  Count_XY_PKEY[PKEY == "MANBBA:14NG49:508896:1:11" & SPECIES == "CAWA", ]$ABUND <- 6

  # Abundance needs to be melted by species
  # According to Diana Stralberg in Sharepoint: "you need to sum abundance within each PKEY. Each row is a bird, not a count." 
  # (https://sharepoint.ualberta.ca/bam/teamsite/Lists/ProjectDiscussion/DispForm.aspx?ID=833)
  Count_XY_PKEY_cast <- dcast.data.table(data = Count_XY_PKEY, 
                                         formula = PKEY + YYYY + X + Y + ROUND ~ SPECIES, 
                                         value.var = "ABUND", 
                                         fill = NA, fun.aggregate = sum)
  # Right now we don't need info on JULIAN, DURATION and DISTANCE, etc because we are not calculating offsets. 
  # But we can change it here if we want to do this on the fly.
  
  # Exclude columns of species we are not using at the moment:
  colsToKeep <- c("PKEY", "YYYY", "X", "Y", "ROUND",
                  "BBWA", "BLPW", "BOCH", "BRCR", 
                  "BTNW", "CAWA", "CMWA", "CONW", 
                  "OVEN", "PISI", "RBNU", "SWTH", 
                  "TEWA", "WETA", "YRWA")
                  
  Count_XY_PKEY_cast_reduced <- Count_XY_PKEY_cast[, ..colsToKeep]
  # Merge Alberto's Data with original DB. Doing this as Alberto filtered his data for some reason. The original DB has 
  fullData <- merge(Count_XY_PKEY_cast_reduced, AlbertosData, by = "PKEY", all.x = FALSE, all.y = TRUE)
  
  # Once the table is really ready, lapply through the combinations and return them as named lists
  dataUploaded <- lapply(X = combinations, FUN = function(x){
    state <- ifelse(grepl("local", x),"State_P_100","State_P_500")
    stateLetter <- ifelse(state=="State_P_100","L","N")
    agent <- ifelse(grepl("Permanent", x),"Permanent",
                    ifelse(grepl("Transitional", x),"Transitional","Both"))
    undist <- ifelse(grepl("Undisturbed", x),TRUE,FALSE)
    both <- ifelse(grepl("Both", x),TRUE,FALSE)
    dataUploaded <- if (undist == TRUE & both == TRUE) {
      fullData[State_P_100 == 0]} else {
        if (undist == TRUE & both == FALSE) {
          fullData[State_P_100 == 0 &
                     (get(paste0("Agent_", stateLetter)) == agent |
                        get(paste0("Agent_", stateLetter)) == "")]
        } else {
          if (undist == FALSE & both == TRUE) {
            fullData
          } else {
            fullData[get(state) == 0 |
                       get(paste0("Agent_", stateLetter)) == agent]
          }
        }
      }
    
    return(dataUploaded)
  })
  
  names(dataUploaded) <- combinations
  return(dataUploaded)
}


