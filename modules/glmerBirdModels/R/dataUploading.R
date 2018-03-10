# dataUploading

dataUploading <- function(data, combinations){
  
  require(data.table)
  require(googledrive)
  
  data.path <- file.path(getwd(), "modules/glmerBirdModels/data", data)
  
  if (file.exists(data.path)){
    fullData <- suppressWarnings(fread(data.path))}
  else {
    invisible(readline(prompt=paste("Make dure you have the dataset in Google Drives folder 'BAM', and press [enter] to continue",
                                    "\nIf authentication fails, please manually place the dataset file in the folder: ",
                                    file.path(getwd(), "modules/glmerBirdModels/data"))))
    require(googledrive)
    drive_download(file.path("BAM",data), path = file.path(getwd(), "modules/glmerBirdModels/data", data), overwrite = TRUE,verbose = FALSE)
    fullData <- suppressWarnings(fread(file.path(getwd(), "modules/glmerBirdModels/data", data)))
  }
  
  dataUploaded <- list()
  dataUploaded <- lapply(X = combinations, FUN = function(x){

       state <- ifelse(grepl("local", x),"State_P_100","State_P_500")
       stateLetter <- ifelse(state=="State_P_100","L","N")
       agent <- ifelse(grepl("Permanent", x),"Permanent",
                       ifelse(grepl("Transitional", x),"Transitional","Both"))
       undist <- ifelse(grepl("Undisturbed", x),TRUE,FALSE)
       both <- ifelse(grepl("Both", x),TRUE,FALSE)
      
       ifelse(undist==TRUE & both==TRUE,
              dataUploaded[[paste(x)]] <- fullData[State_P_100==0],
              ifelse(undist==TRUE & both==FALSE,
                     dataUploaded[[paste(x)]] <- fullData[State_P_100==0 & get(paste0("Agent_",stateLetter))==agent],
                     ifelse(undist==FALSE & both==TRUE,
                            dataUploaded[[paste(x)]] <- fullData,
                            dataUploaded[[paste(x)]] <- fullData[get(state)==0|get(paste0("Agent_",stateLetter))==agent])))
       
    return(dataUploaded)
  })

    l.dataUploaded <- list()
  for (i in 1:length(dataUploaded)){
  l.dataUploaded[i] <- dataUploaded[[i]]
  }
  names(l.dataUploaded) <- combinations
    
  l.dataUploaded$fullData <- fullData
 
  return(l.dataUploaded)
}


