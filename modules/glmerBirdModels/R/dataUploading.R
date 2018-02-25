# dataUploading
# disturbanceDimension <- c("local", "neighborhood)
# typeDisturbance <- c("Transitional", "Permanent", "Undisturbed", "Both")

dataUploading <- function(data, disturbanceDimension, typeDisturbance){
  
  require(data.table)
  
  fullData <- fread(file.path(getwd(), "data", data))
  
  if (is.null("fullData")){
    require(googledrive)
    drive_download("BAM/Final_points_BEAD.csv", path = file.path(getwd(), "data", data), overwrite = FALSE,
                   verbose = FALSE)
    fullData <- fread(file.path(getwd(), "data", data))
  }
  
  dataUploaded <- list()
  
  for (dd in length(disturbanceDimension)){
    for (td in 1:length(typeDisturbance)){
    }
    
    if (disturbanceDimension[dd]=="local"){
      state <- "State_P_100"
      agent <- "L"
    }
       else {
      state <- "State_P_500"
      agent <- "N"
    }
    
    if (typeDisturbance[td]=="Undisturbed"){
      dataUploaded$localUndisturbed <- fullData[State_P_100==0]
      next
    }
    
    if (typeDisturbance[td]=="Both"){
      dataUploaded$fullData <- fullData
      next
    }
    
    paste0(disturbanceDimension[dd],typeDisturbance[td]) <- fullData[state==0|
                                                                       paste0("Agent_",agent)==typeDisturbance[td]]

    dataUploaded$disturbanceDimension[dd]$typeDisturbance[td] <- get(paste0(disturbanceDimension[dd],typeDisturbance[td]))

   # If this dataUploaded$disturbanceDimension[dd]$typeDisturbance[td] doesn't work, try:
    # distDim <- disturbanceDimension[dd]
    # typeDist <- typeDisturbance[td]
  
    return(dataUploaded)
    
  }
}