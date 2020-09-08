# Boreal Species Chosen by Rosenberg et al., 2019
rosenbergData <- prepInputs(url = "https://drive.google.com/open?id=1CnejLh_83OzXVKf5HORskA9dnqCohKSJ", 
                             destinationPath = tempdir(), targetFile = "popChangeBySpeciesRosenberg_2019.csv",
                             fun = "data.table::fread")
rosenbergData <- rosenbergData[Breeding.Biome == "Boreal Forest", c("species", "sci_name", "bird.group")]
list2Match <- prepInputs(url = "https://drive.google.com/file/d/1lgqdciQum3A2ah_TiHZUVlvLxQGemSYD/view?usp=sharing", 
                         destinationPath = tempdir(),
                         fun = "data.table::fread")
names(rosenbergData) <- c("COMMONNAME", "SCINAME", "bird.group")
speciesRosenbergFull <- merge(rosenbergData, list2Match, by = c("COMMONNAME", "SCINAME"), all.x = TRUE)
# 22:              Nashville Warbler    Oreothlypis ruficapilla   landbird <NA> <NA> <NA>   <NA>  <NA>
# 30:              Tennessee Warbler      Oreothlypis peregrina   landbird <NA> <NA> <NA>   <NA>  <NA>
# TEWA, NAWA 
speciesRosenbergFull[, c("bird.group", "SP", "CONF", "CONF6") := NULL]
speciesRosenbergFull[COMMONNAME == "Nashville Warbler", c("SPEC", "SPEC6", "SCINAME") := list("NAWA", "LEIRUF", "Leiothlypis ruficapilla")]
speciesRosenbergFull[COMMONNAME == "Tennessee Warbler", c("SPEC", "SPEC6", "SCINAME") := list("TEWA", "LEIPER", "Leiothlypis peregrina")]
speciesRosenbergFull$RosenbergUsed <- TRUE
speciesWeUsed <- data.table(SPEC = c("BBWA", "BLPW", "BOCH", "BRCR",
                                     "BTNW", "CAWA", "CMWA", "CONW",
                                     "OVEN", "PISI", "RBNU", "SWTH",
                                     "TEWA", "WETA", "YRWA"),
                            WeUsed = TRUE)
speciesWeUsed <- merge(speciesWeUsed, list2Match, by = "SPEC", all.x = TRUE)
speciesWeUsed[, c("bird.group", "SP", "CONF", "CONF6") := NULL]
speciesRosenbergFull <- merge(speciesRosenbergFull, speciesWeUsed, by = c("SPEC", "SPEC6", 
                                                                          "SCINAME", "COMMONNAME"), all = TRUE)
# Do we have all these species in the BAM data?
# Species that we have offset for (coming from Peter Solymos' file names: (prepared by the loadBAMOffsets module))
speciesWithOffsets <-  c("ALFL", "AMCR", "AMGO", "AMPI", "AMRE", "AMRO", "ATSP", "ATTW", 
                         "BANS", "BAOR", "BARS", "BAWW", "BBCU", "BBMA", "BBWA", "BBWO",
                         "BCCH", "BEKI", "BGGN", "BHCO", "BHVI", "BLBW", "BLJA", "BLPW",
                         "BOBO", "BOCH", "BOWA", "BRBL", "BRCR", "BRTH", "BTBW", "BTNW",
                         "BWWA", "CAWA", "CCSP", "CEDW", "CHSP", "CLSW", "CMWA", "COGR",
                         "CONW", "CORA", "COYE", "CSWA", "DEJU", "DOWO", "DUFL", "DUNL",
                         "EABL", "EAKI", "EAPH", "EATO", "EAWP", "EUST", "EVGR", "FISP",
                         "FOSP", "GCFL", "GCKI", "GCSP", "GCTH", "GRAJ", "GRCA", "GRSP",
                         "GRYE", "GWWA", "HAFL", "HAWO", "HETH", "HOLA", "HOSP", "HOWR",
                         "INBU", "KILL", "LALO", "LCSP", "LEFL", "LEYE", "LISP", "MAWA",
                         "MAWR", "MOBL", "MODO", "MOWA", "NAWA", "NESP", "NOCA", "NOFL",
                         "NOPA", "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PHVI", "PIGR",
                         "PISI", "PIWA", "PIWO", "PUFI", "RBGR", "RBNU", "RBWO", "RCKI",
                         "RECR", "REVI", "RHWO", "ROPI", "ROSA", "RTHU", "RUBL", "RUGR",
                         "RWBL", "SAVS", "SCTA", "SEWR", "SOGR", "SOSA", "SOSP", "SPGR",
                         "SPSA", "SWSP", "SWTH", "TEWA", "TOSO", "TOWA", "TRES", "UPSA",
                         "VATH", "VEER", "VESP", "WAVI", "WBNU", "WCSP", "WETA", "WEWP",
                         "WIPT", "WISN", "WITU", "WIWA", "WIWR", "WOTH", "WTSP", "WWCR",
                         "YBCU", "YBFL", "YBSA", "YEWA", "YHBL", "YRWA", "YTVI")
speciesNotInBAM <- which(!speciesRosenbergFull$SPEC %in% speciesWithOffsets)
speciesRosenbergFull[!speciesNotInBAM]
# Missing only 2 species
# SPEC  SPEC6               SCINAME     COMMONNAME RosenbergUsed WeUsed
# 1: CAJA PERCAN Perisoreus canadensis     Canada Jay          TRUE     NA
# 2: GGOW STRNEB        Strix nebulosa Great Gray Owl          TRUE     NA

# Which species should we run the analysis for?
knitr::kable(speciesRosenbergFull[!speciesNotInBAM, c("SPEC", "SCINAME", "COMMONNAME")])

