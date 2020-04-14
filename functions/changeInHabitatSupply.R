changeInHabitatSupply <- function(tble, RTM, # RTM for number of pixels
                                  whichType, # absolute, proportional
                                  pathToSave,
                                  upload = FALSE){

    species <- names(tble)
  library("data.table")
  library("usefun")
    rasterPaths <- lapply(X = species, FUN = function(BIRD){ # return rasters paths
      rasName <- file.path(pathToSave, paste0(whichType, BIRD))
      if (file.exists(paste0(rasName, ".tif"))) {
        message(crayon::green(paste0("Returning existing ", whichType, " map(s) for ", BIRD)))
        return(paste0(rasName, ".tif"))
      } else {
        tblPath <- file.path(pathToSave, paste0("absolute", BIRD, "table.rds"))
        if (file.exists(tblPath)){
          message(crayon::green(paste0("Returning ", whichType, " table for ", BIRD)))
          dt <- readRDS(tblPath)
        } else {
        message(crayon::yellow(paste0("Creating ", whichType, " map(s) for ", BIRD)))
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
        if (whichType == "absolute"){
          dt[, finalCol := value2011-value0]
        } else {
          dt[, finalCol := (value2011-value0)/value0]
        }
      ras <- raster(RTM)
      ras <- raster::setValues(x = ras, values = dt$finalCol)
      names(ras) <- rasName
      writeRaster(x = ras, filename = rasName, format = "GTiff")
      rm(ras); rm(dt); gc()
      if (upload)
        googledrive::drive_upload(media = paste0(rasName, ".tif"), 
                                  path = googledrive::as_id("12iUWhrSBDS8dDXI42uSFqns_7akFSr1R"))
      gc()
      return(paste0(rasName, ".tif")) # If indiv birds, I return a raster (for both absolute or proportional)
    }
  })
  names(rasterPaths) <- species
  return(rasterPaths)
}
