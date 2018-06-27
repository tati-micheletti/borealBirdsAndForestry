# dataUploading

dataUploading <- function(dataset = sim$dataName, 
                          combinations = sim$combinations){
  
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


