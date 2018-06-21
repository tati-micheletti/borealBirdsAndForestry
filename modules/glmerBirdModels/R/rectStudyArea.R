rectStudyArea <- function(x, hectares) {
  
  latLong <-   sp::CRS("+init=epsg:4326")
  if(is(x, "SpatialPoints")) {
    if(is.na(raster::crs(x))) { raster::crs(x) <- latLong }
  } else {
    x <- sp::SpatialPoints(coords = x)
    raster::crs(x) <- latLong
  }
 
  areaCRS <- sp::CRS(paste0("+proj=lcc +lat_1=",raster::ymin(x)," +lat_2=",raster::ymax(x),
                        #       paste0("+proj=lcc +lat_1=49 +lat_2=77
                        " +lat_0=0 +lon_0=",raster::xmin(x)," +x_0=0 +y_0=0 +ellps=GRS80
                        +units=m +no_defs"))
  
  areaM2 <- hectares * 1e4 * 1.304 # rescale so mean area is close to hectares
  y <- sp::spTransform(x, areaCRS)
  
  radius <- areaM2/2
  
  meanX <- mean(sp::coordinates(y)[,1]) - radius
  meanY <- mean(sp::coordinates(y)[,2]) - radius
  
  minX <- meanX - radius
  maxX <- meanX + radius
  minY <- meanY - radius
  maxY <- meanY + radius
  
  mtx <- matrix(c(minX, maxY, maxX, maxY, maxX, minY, minX, minY, minX, maxY), ncol = 2, byrow = TRUE)
  Sr1 <- sp::Polygon(coords = mtx)
  Srs1 <- sp::Polygons(list(Sr1), "s1")
  outPolygon <- sp::SpatialPolygons(list(Srs1), 1L)
  raster::crs(outPolygon) <- areaCRS

  # outPolygon <- sp::spTransform(x = outPolygon, CRS = sp::CRS("+proj=aea +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  return(outPolygon)
}
