
dir.create(file.path(getwd(), "testingPrepInputs"), showWarnings = FALSE)
tmpDir <- file.path(getwd(), "testingPrepInputs")

set.seed(1234)
rP <- SpaDES.tools::randomPolygon(x = matrix(c(-93.028935, 50.271979), ncol = 2), 
                                  hectares = 5000000)
birdSp <- c("BBWA", "YRWA")
birdDensityRasters <- lapply(X = birdSp, FUN = function(x){
  ras <- reproducible::prepInputs(targetFile = file.path(tmpDir, paste0("/", x, "_currmean.asc")),
                    archive = paste0(x, "_current.zip"),
                    url = paste0("https://s3-us-west-2.amazonaws.com/bam-databasin-climatechangepredictions/climatepredictions-ascii/",
                                 x, "_current.zip"),
                    destinationPath = tmpDir,
                    studyArea = rP)
})

disturbanceYear <- reproducible::prepInputs(targetFile = file.path(tmpDir, "C2C_change_year.tif"),
                                            url = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Year.zip",
                                            archive = "C2C_change_year.zip",
                                            alsoExtract = "similar",
                                            rasterToMatch = birdDensityRasters[[1]],
                                            destinationPath = tmpDir,
                                            studyArea = rP)

disturbanceType <- reproducible::prepInputs(targetFile = file.path(tmpDir, "C2C_change_type.tif"),
                                            archive = "C2C_change_type.zip",
                                            alsoExtract = "similar",
                                            url = "https://opendata.nfis.org/downloads/forest_change/C2C_Change_Type.zip",
                                            rasterToMatch = birdDensityRasters[[1]],
                                            destinationPath = tmpDir,
                                            studyArea = rP)
