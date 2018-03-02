fetchData <- function(birdSp) {
  
  dataZipList <- list(BBWA = BBWA,
                      BLPW = BLPW,
                      BOCH = BOCH,
                      BRCR = BRCR,
                      BTNW = BTNW,
                      CAWA = CAWA,
                      CMWA = CMWA,
                      CONW = CONW,
                      OVEN = OVEN,
                      PISI = PISI,
                      RBNU = RBNU,
                      SWTH = SWTH,
                      TEWA = TEWA,
                      WETA = WETA,
                      YRWA = YRWA)
  
  for (i in 1:length(birdSp)) {

    temp <- tempfile()
    dataDir <- file.path(getwd(),"modules/birdDensityBCR_Prov_LCC/data")
    download.file(paste0("https://s3-us-west-2.amazonaws.com/bam-databasin-climatechangepredictions/climatepredictions-ascii/",
                         birdSp[i],"_current.zip",temp))
    unzip(temp, exdir = dataDir)
    dataZipList[[paste0(birdSp[i])]] <- raster::raster(file.path(paste0(dataDir,birdSp,".asc")))
  }
  return(dataZipList)
  }
