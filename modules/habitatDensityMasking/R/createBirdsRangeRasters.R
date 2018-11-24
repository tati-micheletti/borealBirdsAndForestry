createBirdsRangeRasters <- function(birdSpecies = sim$birdSpecies){
  rangeList <- list(
    "BBWA" = "https://drive.google.com/open?id=1OlfDr3jfAN5GpVL_VFlun7c_6hlkRf_x",
    "BLPW" = "https://drive.google.com/open?id=1Lddk59-IEu3L4eh69vIDBHK-lb65r5Bq",
    "BOCH" = "https://drive.google.com/open?id=1iRz8suFkCzmvEeF_RtRZIq0ribRgqIbr",
    "BRCR" = "https://drive.google.com/open?id=1cKi5kGA1fPb1hr5CeGyqA2Hxz7r6Xmhp",
    "BTNW" = "https://drive.google.com/open?id=17fV7GuAv-ORRKU9BmFWAt_TEFqJzRMYK",
    "CAWA" = "https://drive.google.com/open?id=1OKypa6HPkxjLqO9xa0ntq1e0K6k1UXh7",
    "CMWA" = "https://drive.google.com/open?id=17eHdOpH2EXa1cHxY7GFXYdXSHJdjz4zD",
    "CONW" = "https://drive.google.com/open?id=14FaNiZGmSpqfug__Lv3wSn1ze6EM-vIJ",
    "OVEN" = "https://drive.google.com/open?id=1u-zIfA6omd2c5bwCiIGJU-mlGYJ1ekRW",
    "PISI" = "https://drive.google.com/open?id=1fVUEQ6g_K23-yorig8hZFJVKf2DSDL92",
    "RBNU" = "https://drive.google.com/open?id=1u1crglbbMGYVjzKgFCPWApfS6stZnCkh",
    "SWTH" = "https://drive.google.com/open?id=1uB1cwm1DqWXkflUB87-vt-1T8M_YA1Bc",
    "TEWA" = "https://drive.google.com/open?id=10_xgk9uAhbRdXxVx_i59ya9WS_kS5fu7",
    "WETA" = "https://drive.google.com/open?id=1p5o1kb_yM02xexR7Ny9cNTh1H-k4NPK8",
    "YRWA" = "https://drive.google.com/open?id=1ypBzddcw79BqnwFqBxaSTKH6svEt3edf"
  )
  subsetList <- rangeList[birdSpecies]
  return(subsetList)
} 