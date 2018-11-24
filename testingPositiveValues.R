focal2011 <- raster::raster(file.path(paths$cachePath, "rastersrastersmergedFocal2011-100Res250m_3.tif"))#sim@.envir$focalYearList$Year2011
BBWA <- raster::raster(file.path(paths$cachePath,"trends100","mergedTrendBBWA100.tif"))#sim@.envir$trends$BBWA
maskedFocalCropped <- reproducible::postProcess(x = focal2011, rasterToMatch = BBWA, maskWithRTM = TRUE)
vals <- getValues(maskedFocalCropped)
vals[vals < 1.1 & vals > 0] <- 10
focalRevalued <- setValues(maskedFocalCropped, vals)

breakpoints <- c(-0.008404093,0,0.005, 11)
colors <- c("lightblue","red", "green")

pixelsOverlapping <- BBWA[BBWA > 0 & maskedFocalCropped>5]
pixelsOverlapping # NULL

# This means that focal pixels that have value > 0 (that I converted all to 10), 
# don't have positive significant trend coefficients. What a relieve!
