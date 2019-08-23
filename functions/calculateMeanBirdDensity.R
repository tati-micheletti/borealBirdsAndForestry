calculateMeanBirdDensity <- function(shape, ras, type){
  library("usefun")
  r <- postProcess(ras, studyArea = shape, filename2 = NULL)
  toAbund <- eval(parse(text = paste(res(ras), collapse = "*")))/10000 # From m2 to ha
  r <- r*toAbund
  summ <- data.table::data.table(sum = sum(r[], na.rm = TRUE),
                                 min = min(r[], na.rm = TRUE),
                                 max = max(r[], na.rm = TRUE),
                                 average = mean(r[], na.rm = TRUE),
                                 median = median(r[], na.rm = TRUE),
                                 sd = sd(r[], na.rm = TRUE),
                                 whichSummary = type)
  rasCropped <- r
  return(list(table = summ,
              raster = r))
}