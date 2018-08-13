# TO DO

# Run for Ontario: Crashing at reprojecting 
# Ideas: Divide into tiles before, reproject, put back together? Maybe prepInputs could do it (new argument?)

# Area of 500 000 was fairly fast to run (@2-3hs)
# Area of 10000000 was .... to run (@ hs) @25Gb of RAM to reproject CAN_NALCMS_LC_30m_LAEA_mmu12_urb05.tif
# We should be able to run it in an area that is at least 5x bigger...

# Make parallel accross work (?)


# Then change in global:
#   1. start = 1985, end = 2011
#   2. testArea = FALSE
#   3. Change nx and ny parameters
#   4. Activate parallel useParallel = "across"
#   5. On putty:  tail -f /mnt/storage/borealBirdsAndForestry/cache/logParallel (IP: 132.156.149.44 port 22 tmichele)
