# type can be "density" or YYYY (as numeric, i.e. 1985)
# fullTableFilename : RDS object to be saved: table with bird + all pixel ID + value
# summarizedTableFileName : RDS object to be saved: table with summarized numberPixels, areaHa, totalAreaHa, `abundXXXX` or `realAbund0`
# whichToLoad : should be `fullTable` or `summarizedTable`. Default to `summarizedTable`
returnBirdAbundance <- function(filepath, 
                                type, 
                                fullTableFilename, 
                                summarizedTableFileName, 
                                whichToLoad = "summarizedTable", 
                                onlyNA = FALSE, 
                                lightLoad = FALSE, 
                                tablePerPixel = NULL, 
                                rasterToMatch = NULL,
                                overwriteInternals = FALSE){
  
  if (length(filepath)==0) stop("'filepath' needs to be provided")
  if (any(overwriteInternals, 
          all(!file.exists(fullTableFilename), whichToLoad != "summarizedTable"), 
          all(!file.exists(summarizedTableFileName), whichToLoad == "summarizedTable"))){
    message(crayon::yellow(basename(fullTableFilename), " or ", 
                           basename(summarizedTableFileName), " does not exist. ",
                           "Creating and saving"))
    source('/mnt/data/Micheletti/borealBirdsAndForestry/functions/areaAndAbundance.R')
    denRasters <- Cache(areaAndAbundance, 
                        filepath = filepath, 
                        onlyNA = onlyNA, 
                        type = type, 
                        tablePerPixel = tablePerPixel, 
                        rasterToMatch = rasterToMatch)
    
    names(denRasters) <- usefun::substrBoth(strng = tools::file_path_sans_ext(filepath), 
                                                        howManyCharacters = 4, 
                                            fromEnd = TRUE)
    birdsCleanDensities <- data.table::rbindlist(lapply(denRasters, `[[`, "birdDT"))
    if (any(!file.exists(fullTableFilename), 
            overwriteInternals)) saveRDS(object = birdsCleanDensities, 
                                                 file = fullTableFilename)
    birdsTable <- data.table::rbindlist(lapply(denRasters, `[[`, "dt"))
    if (any(!file.exists(summarizedTableFileName), 
            overwriteInternals)) saveRDS(object = birdsTable, 
                                                       file = summarizedTableFileName)
    if (whichToLoad == "fullTable"){
      return(birdsCleanDensities)
    } else {
      return(birdsTable)
    }
  } else {
    message(crayon::green(basename(fullTableFilename), "exists. Returning"))
    if (whichToLoad == "fullTable"){
      if (lightLoad) return(fullTableFilename)
      return(readRDS(fullTableFilename))
    } else {
      if (lightLoad) return(summarizedTableFileName)
      return(readRDS(summarizedTableFileName)) 
    }
  }
}