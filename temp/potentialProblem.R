# WE MIGHT HAVE A PROBLEM THAT...
# It looks like I have a few places with LCC == 0, BCR and PROV == NA, but that we have density and predictions. 
# That might be because I have density (maybe artifacts?) and focal?
# Ok, now I see... The density rasters were created using LCC05 as raster template. However, if I make a rasterToMatch for them with lcc05, it disalign?
# Maybe that's why I have some predictions in pixels that i don't have LCC/BCR/PROV cover. Potentially the predictions were snapped one column and one row less than this
# Maybe not...
# REVISE fetchData when building densityBIRD.tif. It looks like it might be disaligned from the BCR-LCC-PROV raster... even though the map looks good... so maybe?
# If this is right (which is highly likely as I super check data.table operations), then why do i have predictions AND density in places with LCC == 0 and BCR-PROV == NA
# If this is wrong, I have to re-run all densityRaster creations + predictions + posthoc ==> Will run for about 3 weeks...



c("669bd1c9ecc2d1866b02027a97e36f36", "89270f919890c596565a0cf3b61dcd23", "b91ea60825106b0947d7b6ebaeb3f302", "d960ae50e0741072864ea1af6ff28ee8")

# 69e2b514654d7dd1eaa9736950598bbd :: maybe this is the object that corresponds to 
BCR_PROV_LCC_Estimates <- Cache(createBCR_PROV_LCC_Estimates, BCR = BCR, LCC05 = LCC05, 
                               densityEstimates = densityEstimates,
                               userTags = "objectName:BCR_PROV_LCC_Estimates")
# This one might be too. They have different densityEstimates: 6fa7dabae9408323ee2bc11cefc48ea7
# Or this with a different LCC2005: 8cbd4062ba8e8b42c3518de5de303370
# Or this with improved digested algorithm 84a6ad15914517de sept
# Or this one with different density estimates and different LCC05 : be2f601c894428eb4630412f7fb581fa (29/04)

# Maybe the solution is as simple as adding to line 390
originalDensPostProc <- Cache(reproducible::postProcess, originalDens, #rasterToMatch = BCRLCC05$LCC05, 
                              userTags = c("script:paperPlots", "goal:snappingDensityRas", paste0("species:", BIRD)))
# or postprocessing the LCC with my density raster!! YES, that sounds good!