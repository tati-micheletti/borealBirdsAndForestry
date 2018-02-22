# dataUploading

dataUploading <- function(data){
  
require(data.table)
  
  fullTable <- fread(file.path(getwd(), "data", data))
  
  if (is.null("fullTable")){
    require(googledrive)
    drive_download("BAM/Final_points_BEAD.csv", path = file.path(getwd(), "data", data), overwrite = FALSE,
                   verbose = FALSE)
    fullTable <- fread(file.path(getwd(), "data", data))
  }
  
  fullData <- fullTable
  
  localUndist <- fullTable[State_P_100==0]
  
  ###### TRANSITIONAL DISTURBANCES
  # Local scale
  localTrans <- fullTable[State_P_100==0|Agent_L=="Transitional"]
  
  # Neighborhood scale
  neighTrans <- fullTable[State_P_500==0|Agent_N=="Transitional"]
  
  # Neighborhood scale - local intact
  localUndistNeighTrans <- neighTrans[State_P_100==0]
  
  ##### PERMANENT DISTURBANCES
  # Local scale
  localPerm <- fullTable[State_P_100==0|Agent_L=="Permanent"]
  
  # Neighborhood scale
  neighPerm <- fullTable[State_P_500==0|Agent_N=="Permanent"]
  
  # Neighborhood scale - local intact trans
  localUndistNeighPerm <- neighPerm[State_P_100==0]
  
  dataUploaded <- list(localTrans = localTrans, 
                       neighTrans = neighTrans, 
                       localUndistNeighTrans = localUndistNeighTrans, 
                       localPerm = localPerm, 
                       neighPerm = neighPerm,
                       localUndistNeighPerm = localUndistNeighPerm,
                       localUndist = localUndist,
                       fullData = fullData)
  
  return(dataUploaded)
}