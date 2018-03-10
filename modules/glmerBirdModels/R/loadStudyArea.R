# loadStudyArea Function

loadStudyArea <-function(data){
  
  require(rgdal)
  require(sf)
  
  studyArea <- sf::st_read(dsn = file.path(getwd(),"data", data))
  
  return(studyArea)
}