a <- list(A = 50, B = 10, C = 5)
sizeA1 <- utils::object.size(a)
lng <- 1:3
b <- lapply(lng, function(x){
  d <- a[[x]] + 10
  a[[x]] <<- NA
  return(d)
})
sizeA2 <- utils::object.size(a) # Doesn't work. Need to actually remove the object from memory...
