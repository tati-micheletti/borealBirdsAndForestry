# Extracting the values from the rasters to feed the bird predictions
# 
# extractData <- function(datasetRaster = sim$birdDensityRasters){
# 
#   require(data.table)
#   require(raster)
# 
#   browser()
# 
#   if (typeData == "list") {
# 
#     data <- vector(mode = "list", length = length(list))
#     names(data) <- as.list(list)
# 
#     for (i in 1:length(list)){
# 
#       names(data)[i] <- list[i]
# 
#       data[[paste0(list[i])]] <- data.table(sp::coordinates(datasetRaster[[paste0(list[i])]]),
#                                             raster::values(datasetRaster[[i]]))
#       names(data[[paste0(list[i])]]) <- c("X", "Y", "Value")
# 
#     }
# 
#   return(data)
# 
#   } else {
# 
#       data <- data.table(sp::coordinates(datasetRaster),
#                          raster::values(datasetRaster))
#       names(data) <- c("X", "Y", "Value")
# 
#       return(data)
#   }
# 
# }