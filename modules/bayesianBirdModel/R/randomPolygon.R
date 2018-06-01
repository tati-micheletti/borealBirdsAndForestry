randomPolygon <- function(x, hectares) {
  
  latLong <-   sp::CRS("+init=epsg:4326")
  if(is(x, "SpatialPoints")) {
    if(is.na(crs(x))) { crs(x) <- latLong }
  } else {
    x <- sp::SpatialPoints(coords = x)
    crs(x) <- latLong
  }
  
  areaCRS <- CRS(paste0("+proj=lcc +lat_1=",raster::ymin(x)," +lat_2=",raster::ymax(x),
                        #       paste0("+proj=lcc +lat_1=49 +lat_2=77
                        " +lat_0=0 +lon_0=",raster::xmin(x)," +x_0=0 +y_0=0 +ellps=GRS80
                        +units=m +no_defs"))
  
  areaM2 <- hectares * 1e4 * 1.304 # rescale so mean area is close to hectares
  y <- spTransform(x, areaCRS)
  
  radius <- sqrt(areaM2/pi)
  
  meanX <- mean(sp::coordinates(y)[,1]) - radius
  meanY <- mean(sp::coordinates(y)[,2]) - radius
  
  minX <- meanX - radius
  maxX <- meanX + radius
  minY <- meanY - radius
  maxY <- meanY + radius
  
  # Add random noise to polygon
  xAdd <- round(runif(1,radius*0.8, radius*1.2))
  yAdd <- round(runif(1,radius*0.8, radius*1.2))
  nPoints <- 20
  betaPar=0.6
  X = c(jitter(sort(rbeta(nPoints, betaPar, betaPar)*(maxX-minX)+minX)),
        jitter(sort(rbeta(nPoints, betaPar, betaPar)*(maxX-minX)+minX, decreasing = TRUE)))
  Y = c(jitter(sort(rbeta(nPoints/2, betaPar, betaPar)*(maxY-meanY)+meanY)),
        jitter(sort(rbeta(nPoints, betaPar, betaPar)*(maxY-minY)+minY, decreasing = TRUE)),
        jitter(sort(rbeta(nPoints/2, betaPar, betaPar)*(meanY-minY)+minY)))
  
  Sr1 <- Polygon(cbind(X+xAdd,Y+yAdd))
  Srs1 = Polygons(list(Sr1), "s1")
  outPolygon <- SpatialPolygons(list(Srs1), 1L)
  crs(outPolygon) <- areaCRS
  outPolygon <- spTransform(outPolygon, crs(x))
  
  outPolygon <- sp::spTransform(x = outPolygon, CRS = sp::CRS("+proj=aea +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  
  return(outPolygon)
}
