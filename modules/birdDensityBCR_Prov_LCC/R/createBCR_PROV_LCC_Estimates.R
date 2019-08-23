createBCR_PROV_LCC_Estimates <- function(BCR,
                                     LCC05,
                                     densityEstimates = NULL,
                                     justBCRProvLCC = FALSE){
  
  # 2. Rasterize (fasterize) both PROV and BCR.
  # =========== BCR 
  message(crayon::yellow(paste0("Fasterizing BCRsf")))
  BCRsf <- sf::st_as_sf(BCR)
  BCRsf <- BCRsf[!is.na(BCRsf$PROVINCE_S),] # Excluding a weird NA from the polygons
  BCRsf <- BCRsf[BCRsf$COUNTRY == "CANADA",] # Excluding the USA from the analysis
  rasBCR <- fasterize::fasterize(sf = BCRsf, raster = LCC05, field = "BCR")
  rasBCR[] <- rasBCR[]
  
  # # ==========+ PROV
  
  PROV_ID <- data.table(PROVINCE_S = unique(BCRsf$PROVINCE_S), ID = seq(200, 199 + length(unique(BCRsf$PROVINCE_S))))
  BCRsf$PROV_ID <- PROV_ID$ID[match(BCRsf$PROVINCE_S, PROV_ID$PROVINCE_S)]
  rasPROV <- fasterize::fasterize(sf = BCRsf, raster = LCC05, field = "PROV_ID")
  
  # 3. Create a lookout table
  PROVabb <- structure(list(PRENAME = structure(1:13, .Label = c("Alberta", 
                                                                 "British Columbia", "Manitoba", "New Brunswick", "Newfoundland and Labrador", 
                                                                 "Northwest Territories", "Nova Scotia", "Nunavut", "Ontario", 
                                                                 "Prince Edward Island", "Quebec", "Saskatchewan", "Yukon"), class = "factor"), 
                            ID = structure(c(207L, 206L, 200L, 210L, 201L, 202L, 211L, 203L, 209L, 212L, 
                                             204L, 208L, 205L), class = "numeric"), 
                            PROV = structure(c(1L, 2L, 3L, 4L, 5L, 7L, 6L, 8L, 9L, 10L, 
                                               11L, 12L, 13L), .Label = c("AB", "BC", "MB", "NB", "NL", 
                                                                          "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT"), class = "factor")), 
                       .Names = c("PRENAME", "ID", "PROV"), row.names = c(NA, -13L), class = "data.frame")

  if (justBCRProvLCC){
    PROVabb <- data.table::data.table(PROVabb)
    rasPROVvals <- data.table::data.table(pixelID = 1:raster::ncell(LCC05), 
                                          ID = raster::getValues(fasterize::fasterize(sf = BCRsf, raster = LCC05, field = "PROV_ID")))
    provDT <- merge(rasPROVvals, PROVabb, by = "ID", all.x = TRUE)
    setkey(provDT, pixelID)
    PROV_BCR_LCC <- data.table::data.table(PROV = provDT$PROV,
                                           BCR = raster::getValues(rasBCR),
                                           LCC = raster::getValues(LCC05))
    return(PROV_BCR_LCC)
  }
  
  # 4. Set a base data.table to retrieve the specific density values for each pixel 
  PROV_BCR_LCC <- data.table::data.table(ID = as.integer(rasPROV[]),
                                         BCR = as.integer(rasBCR[]),
                                         LCC = as.integer(LCC05[]))
  
  if (is.null(densityEstimates)) stop("densityEstimates can only be null if justBCRProvLCC = TRUE")
  
  densityEstimates <- Cache(plyr::join, densityEstimates, PROVabb, userTags = "densityEstimatesIntegers")
  out <- list(PROV_BCR_LCC = PROV_BCR_LCC, 
              densityEstimates = densityEstimates)
  return(out)
}