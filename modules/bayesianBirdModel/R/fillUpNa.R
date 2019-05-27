fillUpNa <- function(data){
  meanDT <- mean(data, na.rm = TRUE)
  data[is.na(data)] <- rnorm(n = length(data[is.na(data)]), mean = meanDT, sd = 0.001)
  return(data)
}

