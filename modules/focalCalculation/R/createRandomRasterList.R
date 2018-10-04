createRandomRasterList <- function(rastersPerList = 4, 
                                   numberOfLists = 3,
                                   returnPaths = TRUE,
                                   splittedTiles = TRUE) {
  if (splittedTiles == TRUE){
    listRas <- lapply(X = 1:numberOfLists, FUN = function(nList) {
      if (!file.exists(file.path(getwd(), "dummyRasters", paste0("rasterNumber", nList, ".tif")))) {
        gigaRas <- Cache(raster::raster, xmn = -30^4, xmx = 30^4, 
                         ymn = -60^3, ymx = 60^3,
                         resolution = c(30, 30),
                         crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                         vals = round(runif(n = 777600000, min = 1, max = 10)))
        storage.mode(gigaRas[]) <- "integer"
        names(gigaRas) <- paste0("rasterNumber", nList)
        suppressWarnings(dir.create(file.path(getwd(), "dummyRasters")))
        pathToRas <- file.path(getwd(), "dummyRasters", paste0(gigaRas@data@names, ".tif"))
        raster::writeRaster(x = gigaRas, filename = pathToRas, overwrite = TRUE)
        rm(gigaRas); gc()
        return(pathToRas)
    } else {
      return(file.path(getwd(), "dummyRasters", paste0("rasterNumber", nList, ".tif")))
    }
    })

    listTiles <- lapply(X = listRas, FUN = function(eachRaster) {
      sq <- sqrt(rastersPerList)
      if (!sq %% 1 == 0) {
        warning("Square root of rastersPerList (", rastersPerList,
                ") is not an integer. RastersPerList will be updated to generate 9 tiles per list")
        rastersPerList <- 9}
      r <- raster::raster(eachRaster)
      r@data@names <- paste0(r@data@names, "Tiled")
        allLists <- list.files(path = file.path(getwd(), "dummyRasters/tiled"), 
                                      pattern = r@data@names, full.names = TRUE) %>%
          .[grep(pattern = "gri$", x = .)]
          
        if (length(allLists) == rastersPerList) {
          return(allLists)
        } else {
              tilesPaths <- SpaDES.tools::splitRaster(r, nx = sqrt(rastersPerList), ny = sqrt(rastersPerList), buffer = c(18, 18), 
                                path = file.path(getwd(), "dummyRasters/tiled"), rType = "INT1U")
      tilesPathsExtracted <- lapply(X = tilesPaths, FUN = function(path){
        fullPath <- path@file@name
        return(fullPath)
      })
      return(unlist(tilesPathsExtracted))
      }
    })
    names(listTiles) <- paste0("Raster", 1:numberOfLists)
    
  } else {
  # dots <- list(...) # Arguments to pass to raster function [ FIX ]
  # if (is.null(dots)) {
    finalListRas <- lapply(X = 1:numberOfLists, FUN = function(nList) {
      listRas <- lapply(X = 1:rastersPerList, FUN = function(nRas) {
        ras <- raster::raster(xmn = -30^3, xmx = 30^3, 
                              ymn = -30^3, ymx = 30^3,
                       resolution = c(30, 30),
                       crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                       vals = round(runif(n = 3240000, min = 1, max = 10)))
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
        return(listRastersPaths)
      }
      )
      return(listPaths)
    } else {
      return(finalListRas)
    }
  }
  
  return(listTiles)
}
