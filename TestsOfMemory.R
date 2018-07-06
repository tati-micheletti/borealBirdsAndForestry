a <- list(A = raster::raster(nrow = 500000000, ncol = 500000000, 
                             resolution = 1, vals = rnorm(64800, mean = 4, sd = 2)), 
          B = raster::raster(nrow = 500000000, ncol = 500000000,  
                             resolution = 1, vals = rnorm(64800, mean = -4, sd = 2)), 
          C = raster::raster(nrow = 500000000, ncol = 500000000,
                             resolution = 1, vals = rnorm(64800, mean = 0.5, sd = 0.1)))
sizeA1 <- utils::object.size(a)
lng <- 1:3
b <- lapply(lng, function(x){
  d <- a[[x]] + 10
  a[[x]] <<- NA
  return(d)
})

sizeA2 <- utils::object.size(a)
