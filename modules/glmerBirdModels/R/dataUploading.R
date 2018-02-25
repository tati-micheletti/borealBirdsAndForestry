# dataUploading
# disturbanceDimension <- c("local", "neighborhood)
# typeDisturbance <- c("Transitional", "Permanent", "Undisturbed", "Both")

dataUploading <- function(data, disturbanceDimension, typeDisturbance){
  
  require(data.table)
  require(googledrive)
  
  data.path <- file.path(getwd(), "modules/glmerBirdModels/data", data)
  
  if (!is.null(data.path)){
  fullData <- fread(data.path)}
   else {
    require(googledrive)
    drive_download("BAM/Final_points_BEAD.csv", path = file.path(getwd(), "modules/glmerBirdModels/data", data), overwrite = FALSE, verbose = FALSE)
    fullData <- fread(file.path(getwd(), "modules/glmerBirdModels/data", data))
  }
  
  browser() # dataUploading::Fix
  # td is not being a counter from 1:length(typeDist.). It is starting in 4 and staying. Therefore several DS are not being saved...
  
  dataUploaded <- list()
  
  for (dd in 1:length(disturbanceDimension)){
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