# type can be "density" or YYYY (as numeric, i.e. 1985)
# fullTableFilename : RDS object to be saved: table with bird + all pixel ID + value
# summarizedTableFileName : RDS object to be saved: table with summarized numberPixels, areaHa, totalAreaHa, `abundXXXX` or `realAbund0`
# whichToLoad : should be `fullTable` or `summarizedTable`. Default to `summarizedTable`
returnBirdAbundance <- function(filepath, type, fullTableFilename, summarizedTableFileName, 
                                whichToLoad = "summarizedTable", lightLoad = FALSE){
  if (length(filepath)==0) stop("'filepath' needs to be provided") 
  if (any(!file.exists(fullTableFilename), !file.exists(summarizedTableFileName))){
    message(crayon::yellow(basename(fullTableFilename), " or ", 
                           basename(summarizedTableFileName), " does not exist. Creating and saving"))
    source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/areaAndAbundance.R')
    denRasters <- areaAndAbundance(filepath = filepath, type = type)
    names(denRasters) <- usefun::substrBoth(strng = tools::file_path_sans_ext(filepath), 
                                                        howManyCharacters = 4, fromEnd = TRUE)
    birdsCleanDensities <- data.table::rbindlist(lapply(denRasters, `[[`, "birdDT"))
    if (!file.exists(fullTableFilename)) saveRDS(object = birdsCleanDensities, file = fullTableFilename)
    birdsTable <- data.table::rbindlist(lapply(denRasters, `[[`, "dt"))
    if (!file.exists(summarizedTableFileName)) saveRDS(object = birdsTable, file = summarizedTableFileName)
    if (whichToLoad == "fullTable"){
      return(birdsCleanDensities) 
    } else {
      return(birdsTable) 
    }
  } else {
    message(crayon::green(basename(fullTableFilename), " and/or ", 
                           basename(summarizedTableFileName), " exist. Returning"))
    if (whichToLoad == "fullTable"){
      if (lightLoad) return(fullTableFilename)
      return(readRDS(fullTableFilename))
    } else {
      if (lightLoad) return(summarizedTableFileName)
      return(readRDS(summarizedTableFileName)) 
    }
  }
}