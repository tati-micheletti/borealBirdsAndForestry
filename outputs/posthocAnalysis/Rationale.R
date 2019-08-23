# Finishing up the backcasting analysis:
# 1. Calculate prediction area for each bird species: `birdSpecies` in x, `predArea` in y
# 2. Estimate total abundance from Peter&Diana's paper for this area * 6.25: `realAbund0`
# 3. Make one more prediction: set disturbance to zero completely. `Abund0` # ASSUMING pre1985 there is no disturbance
# 4. Calculate the rate of change from 1984 to 1985: `rate1985`(using `(1985-1984)/1984`) (THIS PER PIXEL? JUST TOTAL for now...)
     # Rinse and repeat for all pairs of comparison: `rate1986`, `rate1987`, ..., `rate2011` (using `(1986-1985)/1985`) -- rate of habitat supply loss
     # Rinse and repeat for comparisons to Abund0 (i.e. `(XXXX-Abund0)/Abund0`): `cummRate1985`, `cummRate1986`, ..., `cummRate2011` 
# 5. Calculate the AbundXXXX in each year: `AbundXXXX` (using `realAbund0`*`cummRateXXXX`)
# Table: birdSpecies vs. realAbund0 | rate1985 | ... | rate2011 |  Abund1985 | ... | Abund2011
# 6. Figure out roughly how much had been harvested pre-1984 in the boreal -- support our claim on reduced forestry activities pre1984

# PLOTS AND MAPS TO PUT TOGETHER
# PLOT1: rate of decrease for each year -- is forestry reducing? (DOES PIXEL vs TOTAL AGREE?)
# PLOT2: How much did habitat supply change from 1984 to the given year (cummulative rate of habitat loss) # Exclusively related to forest activities
# MAP 1: Map of change in disturbance: (mergedFocal2011-mergedFocal0)/mergedFocal0 -- only works if these are cummulative! 
   # mergedFocal0 <- mergedFocal2011
   # mergedFocal0[!is.na(mergedFocal0)] <- 0
# MAP2: Change in habitat supply (absolute number -- realAbund2011-realAbund0) for each species
# MAP3: Change in habitat supply (proportional -- (realAbund2011-realAbund0)/realAbund0) for each species (Especially SAR?)
# Proportion of species that showed significant decline? SAR? -- think
# PLOT3: For each species, check if the decrease is happening in areas where te birds are already not too abundant:
   # Expected density for each combination of BCR_Prov_LCC (densityBIRD.tif) vs MAP3 (pixel by pixel approach): correlation test plot
# PLOT4: For each species, the percentage of pixels that showed significant decline: 
   # significant <- sum(significantSlope[significantSlope > 0])
   # allNotNA <- sum(significantSlope[!is.na(significantSlope)])
   # allNA <- sum(significantSlope[is.na(significantSlope)])
   # nonSignificant <- sum(significantSlope[significantSlope == 0])
   # if (!all(significant + nonSignificant == allNotNA, 
   #      significant + nonSignificant + allNA == ncell(significantSlope),
   #      allNotNA + allNA == ncell(significantSlope)))
   #   stop("Something is not adding up. Debug") 
  