changeInHabitatSupplyAll <- function(tble, RTM, # RTM for number of pixels
                                  whichType, # absolute, proportional
                                  pathToSave,
                                  upload = FALSE){
    species <- names(tble)
    rasName <- file.path(pathToSave, paste0(whichType, "all")) # in this case NEEDS to be proportionalall or absoluteall
    if (file.exists(paste0(rasName, ".tif"))){
      message(crayon::green(paste0("Returning existing ", whichType, " map(s) for all")))
      return(paste0(rasName, ".tif"))
    } else {
  library("data.table")
  library("usefun")
      finalTablePath <- file.path(pathToSave, paste0("proportionalall_FinalTable.rds"))
      if (file.exists(finalTablePath)) {
        message(crayon::green(paste0("Returning existing ", finalTablePath, " for all")))
        dtEachBird <- readRDS(finalTablePath)
        } else {
  dtEachBird <- lapply(X = species, FUN = function(BIRD){ # return dt
      tblPath <- file.path(pathToSave, paste0("absolute", BIRD, "table.rds"))
      if (file.exists(tblPath)){
        message(crayon::yellow(paste0("Returning ", whichType, " table for ", BIRD)))
        return(readRDS(tblPath))
      } else {
        message(crayon::yellow(paste0("Creating ", whichType, " table for ", BIRD)))
        tbl <- readRDS(tble[[BIRD]])
        # Add missing pixels
        ras <- raster(RTM)
        pixels <- data.table(pixelID = 1:ncell(RTM))
        vals  <- data.table(pixelID = tbl[, pixelID],
                            value2011 = tbl[, realAbund2011], 
                            value0 = tbl[, realAbund0])
        rm(tbl); gc()
        dt <- merge(pixels, vals, all.x = TRUE)
        saveRDS(dt, tblPath)
      }
          return(dt)
  })
  message(crayon::cyan(paste0("Saving dtEachBird table")))
  saveRDS(dtEachBird, finalTablePath)
  }
  names(dtEachBird) <- species
  dt <- usefun::cbindFromList(dtEachBird)
  year2011 <- usefun::grepMulti(x = names(dt), patterns = "value2011")
  year0 <- usefun::grepMulti(x = names(dt), patterns = "value0")
  dt[, summed2011 := rowSums(.SD), .SDcols = year2011]
  dt[, summed0 := rowSums(.SD), .SDcols = year0]
  if (whichType == "proportional"){
   dt[, change := (summed2011-summed0)/summed0]
  } else {
   dt[, change := summed2011-summed0]
  }
  ras <- raster(RTM)
  ras <- raster::setValues(x = ras, values = dt[, change])
  writeRaster(x = ras, filename = rasName, format = "GTiff")
  if (upload)
    googledrive::drive_upload(media = paste0(rasName, ".tif"), path = googledrive::as_id("12iUWhrSBDS8dDXI42uSFqns_7akFSr1R"))
  return(paste0(rasName, ".tif")) # If all birds, I return a raster for proportional
    }
}
