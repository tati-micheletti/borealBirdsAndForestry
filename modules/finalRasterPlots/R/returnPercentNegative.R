returnPercentNegative <- function(x) {
  num <- length(x[!x == 0])
  denom <- length(x[(!is.na(x))])
  ans <- round(num/denom, digit = 3)
  return(ans)
}