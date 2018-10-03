createRandomRasterList <- function(rastersPerList = 4, 
                                   numberOfLists = 3,
                                   returnPaths = TRUE) {
  # dots <- list(...) # Arguments to pass to raster function
  # if (is.null(dots)) {
    finalListRas <- lapply(X = 1:numberOfLists, FUN = function(nList) {
      listRas <- lapply(X = 1:rastersPerList, FUN = function(nRas) {
        ras <- raster::raster(xmn = -30^3, xmx = 30^3, 
                              ymn = -30^3, ymx = 30^3,
                       resolution = c(30, 30),
                       crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                       vals = round(runif(n = 3240000, min = 3, max = 10)))
        names(ras) <- paste0("rasterNumber", nRas, "List", nList)
        return(ras)
      })
      names(listRas) <- paste0("rasterNumber", 1:rastersPerList)
      return(listRas)
    })
    names(finalListRas) <- paste0("rasterList", 1:numberOfLists)
  # } else {
  #   finalListRas <- lapply(X = 1:numberOfLists, FUN = function(nList) {
  #     listRas <- lapply(X = 1:rastersPerList, FUN = function(nRas) {
  #       ras <- do.call(what = raster::raster, args = list(dots))
  #       names(ras) <- paste0("rasterNumber", nRas)
  #       return(ras)
  #     })
  #     names(listRas) <- paste0("rasterNumber", 1:rastersPerList)
  #     return(listRas)
  #   })
  #   names(finalListRas) <- paste0("rasterList", 1:numberOfLists)
  # }

    if (returnPaths == TRUE) {
      listPaths <- lapply(finalListRas, FUN = function(rasList) {
        listRastersPaths <- lapply(X = rasList, FUN = function(ras){
          suppressWarnings(dir.create(file.path(getwd(), "dummyRasters")))
          pathToRas <- file.path(getwd(), "dummyRasters", paste0(ras@data@names, ".tif"))
          raster::writeRaster(x = ras, filename = pathToRas, overwrite = TRUE)
          return(pathToRas)
        })
      }
      )
      return(listPaths)
    } else {
      return(finalListRas)
    }

}
