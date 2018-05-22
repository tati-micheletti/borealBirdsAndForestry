datasetUploading <- function(dataPath = dataPath(sim), data = sim$dataName){
  
  require(data.table)
  require(googledrive)
  
  if (file.exists(file.path(dataPath, data))){
    fullData <- suppressWarnings(fread(file.path(dataPath, data)))
    
    } else {

    invisible(readline(prompt=paste("Make dure you have the dataset in Google Drives folder 'BAM/Datasets/borealBirdsAndForestry', and press [enter] to continue",
                                    "\nIf authentication fails, please manually place the dataset file in the folder: \n",
                                    "~borealBirdsAndForestry/modules/bayesianBirdModel/data")))
    require(googledrive)
    
    drive_download(file.path("BAM/Datasets/borealBirdsAndForestry",data), path = file.path(dataPath, data), 
                   overwrite = TRUE,
                   verbose = FALSE)
    fullData <- suppressWarnings(fread(file.path(dataPath, data)))
  }

  return(fullData)
}