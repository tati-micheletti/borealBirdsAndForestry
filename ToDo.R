# TO DO

# Double check all code workflow : delete all plotting and all return of rasters (only paths)

# after finishing, uncomment line 44 in fetchData() to use as template the projection from the disturbance product (hardcoded...)
# When Ian finished and this running finishes, we can try again running for Ontario. If it works, we can run for the country.

# Area of 500 000 was fairly fast to run (@2-3hs)

# Area of 10 000 000 was ok to run (30 hs) @80Gb of RAM once finished and trying to save. how much takes for 600 000 000
# We should avoid making final plots 
# We should be able to run it in an area that is at least 5x bigger... NOPE! =/
# splitting tiles is taking up about up to 62GB of RAM... So mayb 2x bigger max? How doe this memo allocation works?

# TAKE ALL PLOTS FROM THE CODE, LEAVE ONLY PATHS!!!!
# 

# Make parallel accross work (?)


# Then change in global:
#   1. start = 1985, end = 2011
#   2. testArea = FALSE
#   3. Change nx and ny parameters
#   4. Activate parallel useParallel = "across"
#   5. On putty:  tail -f /mnt/storage/borealBirdsAndForestry/cache/logParallel (IP: 132.156.149.44 port 22 tmichele)
