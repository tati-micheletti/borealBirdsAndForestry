# dataUploading

# On 27JUL18 I addded lines from the SQL as I figured Alberto also has mistakes in his abundances calculations.

# On 25JUL18 I "reverted" the dataUpload commit from what I was working on Alberto's data to the original (didn't revert, just re-wrote the file 
# as I didn't want to mess up the other files). 
# There is a tag to the file from the commit: dataUploadingOriginalWorking

dataUploading <- function(dataset = sim$dataName, 
                          combinations = sim$combinations,
                          birdDensityDS = sim$birdDensityDS,
                          offsetDT = sim$offsetsBySpecies,
                          avoidAlbertosData = P(sim)$avoidAlbertosData,
                          SQLTableName = "SQLData",
                          envirSim = envir(sim)){
  
  require(data.table)
  require(googledrive)

  data.path <- file.path(getwd(), "modules/glmerBirdModels/data", dataset)
  
  if (file.exists(data.path)){
    fullData <- suppressWarnings(fread(data.path))}
  else {
    invisible(readline(prompt=paste("Make dure you have the dataset in Google Drives folder 'BAM/Datasets/borealBirdsAndForestry', and press [enter] to continue",
                                    "\nIf authentication fails, please manually place the dataset file in the folder: \n",
                                    file.path(getwd(), "modules/glmerBirdModels/data"))))
    require(googledrive)
    drive_download(file.path("BAM/Datasets/borealBirdsAndForestry", dataset), path = file.path(getwd(), "modules/glmerBirdModels/data", dataset), overwrite = TRUE,verbose = FALSE)
    fullData <- suppressWarnings(fread(file.path(getwd(), "modules/glmerBirdModels/data", dataset)))
  }
  
if (avoidAlbertosData == FALSE){
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
} else {
  # Adding to Alberto's data the correct densities and offsets
  
  # Preparing Expected densities
  birdnames <- unique(birdDensityDS$SPECIES)
  birdDensityDS$LCC_PROV_B <-
    paste0(birdDensityDS$LCC, birdDensityDS$PROV, birdDensityDS$BCR)
  birdDensityDS_cast <- dcast.data.table(
    data = birdDensityDS,
    formula = LCC_PROV_B ~ SPECIES,
    value.var = "D",
    fill = NA)
  names(birdDensityDS_cast)[names(birdDensityDS_cast) %in% birdnames] <-
    paste0("DENSITY_", birdnames)
  birdDS_Full <-
    merge(fullData,
          birdDensityDS_cast,
          by = "LCC_PROV_B",
          all.x = TRUE,
          all.y = FALSE)
  
  # Joining Offsets
  birdnames <- names(offsetDT)[which(!names(offsetDT) == "PKEY")]
  names(offsetDT)[names(offsetDT) %in% birdnames] <- paste0("OFFSET_", birdnames) # TEMP
  fullData <- merge(birdDS_Full, 
                    offsetDT,
                    by = "PKEY",
                    all.x = TRUE,
                    all.y = FALSE)

    # Retrieve the SQL data
      Count_XY_PKEY_orig <- get(SQLTableName, envir = envirSim)
    
      # Cleaning as per Alberto ("Steps to create database.docx" located in his Dropbox folder called )
      # 2. Merging counts and XY
      Count_XY <- merge(Count_XY_PKEY_orig$PtCount, Count_XY_PKEY_orig$XY, by = "SS", all.x = TRUE, all.y = FALSE) # Obs.: we have several PCODE in XY that are NA's. However, PtCount has the info. 
      names(Count_XY)[names(Count_XY) == "PCODE.x"] <- "PCODE"
      Count_XY <- Count_XY[,!"PCODE.y"]
      
      # 2. Exclude rows without X or Y from Count-XY (n = 80); from  entriees we should end up with . All other columns have all info except BEH (which we don't use now).
      Count_XY <- Count_XY[!is.na(X),]
      
      # There are several cols that have NA (but none matter right now to us): ROUND = 7184; JULIAN = 62194; SITE = 2875; STN = 2877
      # 3. Merge it with PKEY
      Count_XY_PKEY <- merge(Count_XY, Count_XY_PKEY_orig$PKEY, by = c("PKEY", "SS", "PCODE"), all.x = TRUE, all.y = FALSE) # 2 values are inconsistent in PKEY (PKEY = "CL:F-100-1:4-3:14:0650", "CL:F-100-3:1-1:12:c") Seems as typos
      
      # Need to filter to exclude ONE ficticious speciesthat was called "YYYY": 
      #          PKEY         SS    PCODE  SPECIES ABUND DURATION DISTANCE BEH   X       Y      METHOD   ROUND YYYY JULIAN SITE STN
      #  1: JNP:11:8:10:1 JNP:11:8   JNP    YYYY     1       16        7   1 -118.2602 52.74181 JNP:114     1  2010      0   11   8
      Count_XY_PKEY <- Count_XY_PKEY[!SPECIES == "YYYY", ]
    
      # # CAWA (among others, but we don't use other species) has a typo. CAWA's is in PKEY "MANBBA:14NG49:508896:1:11". 
      # Correct this one to 6 as per Alberto's suggestion.
      # fullData[PKEY == "MANBBA:14NG49:508896:1:11" & SPECIES == "CAWA", ]$ABUND <- 6
      # OBS. Alberto doesn't have it in his ds...
      
      Count_XY_PKEY[PKEY == "MANBBA:14NG49:508896:1:11" & SPECIES == "CAWA", ]$ABUND <- 6
  
      # Abundance needs to be melted by species
        # According to Diana Stralberg in Sharepoint: "you need to sum abundance within each PKEY. Each row is a bird, not a count." 
          # (https://sharepoint.ualberta.ca/bam/teamsite/Lists/ProjectDiscussion/DispForm.aspx?ID=833)
          Count_XY_PKEY_cast <- dcast.data.table(data = Count_XY_PKEY, formula = PKEY + YYYY + X + Y + PCODE + ROUND ~ SPECIES, 
                                                                       value.var = "ABUND", 
                                                                       fill = NA, fun.aggregate = sum)
        # Right now we don't need info on JULIAN, DURATION and DISTANCE, etc because we are not calculating offsets. 
        # But we can change it here if we want to do this on the fly.
          
  # From  dataset, we need to remove  first  visit  (Round  =  1)  from  the  Callin  Lake  subset (PCODE  =  CL):
  # As I used Alberto's PKEY's to subset, these will be excluded anyway. Will add it here anyway, as it is a quick operation.
          subExclude <- Count_XY_PKEY_cast[PCODE =="CL" & ROUND == 1]
          Count_XY_PKEY_cast_noCall <- Count_XY_PKEY_cast[!subExclude,]

      # Subset the whole dataset for the species we are working with
        birdSp <- unique(birdDensityDS$SPECIES)
        colsToKeepSQL <- c("PKEY", birdSp)
        Count_XY_PKEY_cast_reduced <- Count_XY_PKEY_cast_noCall[, ..colsToKeepSQL]
        
        # Merge Alberto's Data with original DB. Doing this as Alberto filtered his data for forested areas and did his GIS. 
        # I have to keep the same samples.

        fullData <- merge(Count_XY_PKEY_cast_reduced, fullData, 
                                 by = "PKEY",
                                 all.x = FALSE, all.y = TRUE)
        # OBS:
        # Merging using YYYY, X and Y too doesn't work because: 
        # 1. Count_XY_PKEY_cast_reduced has NA's in YYYY: which(is.na(Count_XY_PKEY_cast_reduced$YYYY)) #49981 52003
        # 2. X and Y: even though they are the same, as one comes from a matrix R identifies as they being different I guess. 
        # Therefore, use only PKEY to merge (as it is a character and can't change. X and Y are numeric) 
        
        # As per Alberto's/Steve's suggestion: if a cluster has a species present at least once, 
        # the points in the cluster for this species that are NA become 0. This is regardless of 
        #the year it was observed.

        # Updating the values to 0 in clusters that have at least 1 observation
        nonZeros <- fullData[, lapply(.SD, FUN = sum, na.rm = TRUE), by = ClusterSP, .SDcols = birdSp]
        clustersSP <- unique(fullData$ClusterSP)
        invisible(lapply(X = clustersSP, function(cluster){
          spClusterToChange <- birdSp[nonZeros[ClusterSP == cluster, .SD > 0, .SDcols = birdSp]]
          lapply(X = spClusterToChange, FUN = function(sp){
            fullData[ClusterSP == cluster & is.na(get(sp)), (sp) := 0]
          })
        })
        )
        densityNames <- names(fullData)[grep(pattern = "DENSITY_", x = names(fullData))]
        lapply(X = densityNames, FUN = function(density_Species){
          logNames = paste0("log", density_Species)
          fullData[ , (logNames) := log(eval(parse(text = density_Species)))]
          })

   dataUploaded <- lapply(
    X = combinations,
    FUN = function(x) {
      state <- ifelse(grepl("local", x), "State_P_100", "State_P_500")
      stateLetter <- ifelse(state == "State_P_100", "L", "N")
      agent <- ifelse(grepl("Permanent", x),
                      "Permanent",
                      ifelse(grepl("Transitional", x), "Transitional", "Both"))
      undist <- ifelse(grepl("Undisturbed", x), TRUE, FALSE)
      both <- ifelse(grepl("Both", x), TRUE, FALSE)
      dataUploaded <- if (undist == TRUE & both == TRUE) {
        fullData[State_P_100 == 0]
      } else {
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
    }
  )
  
  names(dataUploaded) <- combinations
  
}
  return(dataUploaded)
}
