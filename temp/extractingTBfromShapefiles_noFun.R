# Adding bird table to shapefiles

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# MANAGED FOREST VS NON-MANAGED FOREST #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now I can put together the managedForest raster and the birds table
pixList_managed <- lapply(names(pixelTablesWithUncertaintyPre05), function(sp){
  tabSP <- readRDS(pixelTablesWithUncertaintyPre05[[sp]])
  managedForestDTnoNA <- na.omit(managedForestDT) # Remove areas outside of the boreal (where we have NA)
  setkey(managedForestDTnoNA, "pixelID")
  setkey(tabSP, "pixelID")
  tab <- merge(tabSP, managedForestDTnoNA, by = "pixelID")
  # Assertion: In this case, we shouldn't have already any predictions happening outside of the 
  # Managed or unmanaged forests. So NROW(tabSP) == NROW(tab)
  # HOWEVER: we have a 264 pixels difference, which I believe is ok (rounding corners of predictions/shapefile)
  message(paste0("Merge finished for managed and unmanaged forests for ", sp))
  tbName <- file.path(folderForTables, 
                      paste0("finalPixelTable_forSummary_", sp, spatialScale, "m.rds"))
  saveRDS(tab, file = tbName)
  rm(tab); gc()
  return(tbName)
})
names(pixList_managed) <- species

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# MANAGED FOREST FOR EACH PROVINCE #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'

# Now I can put together the managedForest raster and the birds table
finalPixelTableListBCRPROV <- lapply(names(pixelTablesWithUncertaintyPre05), function(sp){
  tabSP <- readRDS(pixelTablesWithUncertaintyPre05[[sp]])
  setkey(managedForestProvBCR, "pixelID")
  setkey(tabSP, "pixelID")
  tab <- merge(tabSP, 
               managedForestProvBCR, by = "pixelID")
  tbName <- file.path(folderForTables, 
                      paste0("finalPixTab_BCRPROV_forSummary_", 
                             sp,
                             spatialScale,
                             "m.rds"))
  message(paste0("Merge finished for BCR and Province for ", sp))
  saveRDS(tab, file = tbName)
  rm(tab); gc()
  return(tbName)
})
names(finalPixelTableListBCRPROV) <- species

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# STUDY AREA QUEBEC #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now I can put together the quebecSA raster and the birds table
# If a given species is not in Quebec, the tables will be 0 length! 
finalPixelTableListQuebec <- lapply(names(pixelTablesWithUncertaintyPre05), function(sp){
  tabSP <- readRDS(pixelTablesWithUncertaintyPre05[[sp]])
  setkey(quebecSADT, "pixelID")
  setkey(tabSP, "pixelID")
  tab <- merge(tabSP, quebecSADT, by = "pixelID")
  tbName <- file.path(folderForTables, 
                      paste0("finalPixelTable_Quebec_forSummary_", sp, spatialScale, "m.rds"))
  message(paste0("Merge finished for Quebec for ", sp))
  saveRDS(tab, file = tbName)
  rm(tab); gc()
  return(tbName)
})
names(finalPixelTableListQuebec) <- species
#####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# STUDY AREA ALBERTA #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Now I can put together the albertaSA raster and the birds table
finalPixelTableListAlberta <- lapply(names(pixelTablesWithUncertaintyPre05), function(sp){
  tabSP <- readRDS(pixelTablesWithUncertaintyPre05[[sp]])
  setkey(albertaSADT, "pixelID")
  setkey(tabSP, "pixelID")
  tab <- merge(tabSP, albertaSADT, by = "pixelID")
  tbName <- file.path(folderForTables, 
                      paste0("finalPixelTable_Alberta_forSummary_", sp, spatialScale, "m.rds"))
  message(paste0("Merge finished for Alberta for ", sp))
  saveRDS(tab, file = tbName)
  rm(tab); gc()
  return(tbName)
})
names(finalPixelTableListAlberta) <- species

#####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# STUDY AREA CALLING LAKE #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~