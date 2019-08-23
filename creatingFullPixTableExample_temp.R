library("data.table")
fullTablePixels1 <- data.table::data.table(species = rep(c("AAAA", "BBBB"), each = 4), 
                                           pixelID = 1:4,
                                           abund = c(12,43,45,21,567,578,456,876))
fullTablePixels1$year <- "abundear1"
fullTablePixels2 <- fullTablePixels1
fullTablePixels2$abund <- round(fullTablePixels2$abund*rep(c(0.97, 0.92), each =4))
fullTablePixels2$year <- "Year2"
fullTablePixels3 <- fullTablePixels2
fullTablePixels3$abund <- round(fullTablePixels2$abund*rep(c(0.98, 0.95), each =4))
fullTablePixels3$year <- "Year3"
fullTablePixels <- list(fullTablePixels1, fullTablePixels2, fullTablePixels3)
# START CODE
fullTablePixels
fullTablePixels <- lapply(X = 1:length(fullTablePixels), FUN = function(YEAR){
  fullTablePixels[[YEAR]][["year"]] <- paste0("year", YEAR + 1983)
  dcastedTable <- dcast(data = fullTablePixels[[YEAR]], formula = species + pixelID ~ year, value.var = "density")
})
fullTablePixels <- rbindlist(fullTablePixels)

