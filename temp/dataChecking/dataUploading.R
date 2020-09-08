# dataUploading
# disturbanceDimension <- c("local", "neighborhood)
# typeDisturbance <- c("Transitional", "Permanent", "Undisturbed", "Both")

dataUploading <- function(data, disturbanceDimension, typeDisturbance){
  
  require(data.table)
  require(googledrive)
  
  data.path <- file.path(getwd(), data)
  
  if (!is.null(data.path)){
  fullData <- suppressWarnings(fread(data.path))}
   else {
    require(googledrive)
    drive_download(file.path("BAM",data), path = file.path(getwd(), "modules/glmerBirdModels/data", data), overwrite = FALSE, verbose = FALSE)
    fullData <- suppressWarnings(fread(file.path(getwd(), data)))
  }
  
  dataUploaded <- list(localUndisturbed = NA, 
                       fullData = NA,
                       localTransitional = NA, 
                       localPermanent = NA, 
                       neighborhoodTransitional = NA, 
                       neighborhoodPermanent = NA)
  
  for (dd in 1:length(disturbanceDimension)){
    for (td in 1:length(typeDisturbance)){
    
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

      dataUploaded[[paste0(disturbanceDimension[dd],typeDisturbance[td])]] <- 
             fullData[get(paste0(state))==0|get(paste0("Agent_",agent))==typeDisturbance[td]]

  }
}
  return(dataUploaded)
}