# type can be "density" or YYYY (as numeric, i.e. 1985)
# fullTableFilename : RDS object to be saved: table with bird + all pixel ID + value
# summarizedTableFileName : RDS object to be saved: table with summarized numberPixels, areaHa, totalAreaHa, `abundXXXX` or `realAbund0`
# whichToLoad : should be `fullTableFilename` or `summarizedTableFileName`. Default to `summarizedTableFileName`
library("raster")
areaAndAbundance <- function(filepath, type, tablePerPixel = NULL){
  originalDensityRasters <- lapply(X = filepath, FUN = function(rasPath){
    if (type == "density"){
      bird <- usefun::substrBoth(strng = tools::file_path_sans_ext(rasPath), 
                                 howManyCharacters = 4, fromEnd = TRUE) 
    } else {
      bird <- usefun::substrBoth(usefun::substrBoth(strng = basename(rasPath),
                                                    howManyCharacters = 13, fromEnd = FALSE), 
                                 howManyCharacters = 4, fromEnd = TRUE)
    }
    message(crayon::yellow("Calculating area and abundance of ", 
                           bird, " for ", type))
    birdRas <- raster::raster(rasPath)
    birdDT <- data.table::data.table(species = bird, density = raster::getValues(birdRas), pixelID = 1:raster::ncell(birdRas))
    birdDT <- na.omit(birdDT) # Only non-NA pixels
    dt <- data.table::data.table(species = bird, numberPixels = NROW(birdDT), 
                                 areaHa = prod(raster::res(birdRas))/10000)
    print("TOTAL AREA BASED ON tablePerPixel == ", tablePerPixel, " ", ifelse(tablePerPixel, areaHa, NROW(birdDT)*areaHa))
    browser()
    dt[, c("totalAreaHa", ifelse(type == "density", "realAbund0", paste0("abund", type))) := 
         list(ifelse(tablePerPixel, areaHa, NROW(birdDT)*areaHa), sum(birdDT$density*areaHa))]
    message(crayon::green("Finished ", 
                           bird, " for ", type))
    return(list(birdDT = birdDT, dt = dt))
  })
  }
