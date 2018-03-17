# Updated TO DO's

# 1. Check models for convergence. If they don't converge, re-run them without the interaction Cluster:Year [OK] / Fix this module for the data from a table
# 2. Make a module for retrieving Peter's offsets from QPAD for the species and localtions? Check how QPAD works (function load_BAM_QPAD()?)
# Already installed QPAD, and uploaded the offsets. Just don't know how to use them! There is a .RData file in BAM Gdrive (pack_2016-12-01.RData),
#     but I wanted something coming from the QPAD. If not possible, ask Nicole if she can upload this file to BAM's website.
# 3. Make a module to either get info on proportion of disturbance from an "excel file" or from a module (this would be to either 
#     feed glmerBirdModels or predictModule). Already started: module getDisturbanceData, but didn't write much yet.
# 4. Make a module that will put all data together
# 5. Fix log(birdDensityBCR_Prov_LCC)
# 6. Module for dataUploading (abundance, X, Y, Cluster, YYYY)

# 3.4. Need to place plots in the module once all plots are ready 
# TO DO's Alberto's work:
# 1. Double check dataset for duplicates; [OK]
# 2. Run the models in SpaDES with the whole dataset for all species; [OK]
# 3. Redo the graphs / figures; [Partial]
# 3.1 Plot 1: FIgure 2 [OK]
# 3.2 Plot 2: Figure 3 and 4 [OK]
# 3.3 Plot 3: Figure S1 and S2 [OK]
# 3.5 Fix Table Appendix 1 [OK]
# 3.6 Fix Table S1 [OK]
# 4. Correct the text
# 5. Save Figures and Tables separately [partially OK]
# 6. Send it to co-authors


# Suggestions:
# Question 1 is still not clear to me
# 1. Instead of +- standard error, I would calculate the real CI (lmerTest)
# 2. Figure S1, S2: ANOVA on the curves to see signifficance
# 3. I would have tested models with both local and neighborhood
# 4. Compare models regarding random effects (run them all without RE and then ANOVA to see)
# 5. the  estimates for this coefficient should be close to 0 (on a log scale), if the BAM predicted bird densities are unbiased. 
# 6. Statistical part needs to be improved!

# modelsSum <- lapply(mySimOut$models, FUN = function(x){ 
#   bird <- lapply(x, FUN = function(bird){
# #    browser()
#     a <- summary(bird)
#     return(a)})
#     return(bird)
#   })
# 


