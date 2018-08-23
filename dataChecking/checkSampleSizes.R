sampleSize <- lapply(MySimOutOld@.list$data, function(x){
  rws <- nrow(x)
  return(rws)
})