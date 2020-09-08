library(raster)
harvestYears <- raster(file.path(getwd(), "borealBirdsAndForestry/inputs/forestHarvest1985_2015/CA_harvest_year_1985_2015.tif"))
library(quickPlot)
Plot(harvestYears)


library(data.table)
three0 <- pryr::object_size(numeric(length = 1*1))
three1 <- pryr::object_size(numeric(length = 1*10^1))
three2 <- pryr::object_size(numeric(length = 1*10^2))
three1 <- pryr::object_size(numeric(length = 1*10^1))
three4 <- pryr::object_size(numeric(length = 1*10^4))
three5 <- pryr::object_size(numeric(length = 1*10^5))
three6 <- pryr::object_size(numeric(length = 1*10^6))
three7 <- pryr::object_size(numeric(length = 1*10^7))
three8 <- pryr::object_size(numeric(length = 1*10^8))

nine <- integer(length = 1.8*10^9)
pryr::object_size(nine)
nine[1:1*10^9] <- NA
pryr::object_size(nine)


DT <- data.table(nZeros = c(0:9),
                 vecSize = c(unlist(lapply(paste0("three", 0:9), get))))
DT2 <- data.table(vec1 = numeric(length = 1*10^8), 
                 vec2 = seq(1, 1*10^8))

DT3 <- list(A = data.table(vec1 = seq(1, 2*10^8)),
            B = data.table(vec2 = seq(1, 2*10^8)),
            C = data.table(vec3 = seq(1, 2*10^8)))

DT3.3 <- data.table(vec1 = seq(1, 2*10^8),
                        vec2 = seq(1, 2*10^8),
                        vec3 = seq(1, 2*10^8))

DT3.3b <- data.table(vec1 = as.integer(seq(1, 2*10^8)),
                    vec2 = seq(1, 2*10^8),
                    vec3 = seq(1, 2*10^8))

DT3.3c <- data.table(vec1 = as.integer(seq(1, 2*10^8)),
                     vec2 = as.integer(seq(1, 2*10^8)),
                     vec3 = seq(1, 2*10^8))

DT3.3d <- data.table(vec1 = as.integer(seq(1, 2*10^8)),
                     vec2 = as.integer(seq(1, 2*10^8)),
                     vec3 = as.integer(seq(1, 2*10^8)))

DT3.3e <- data.table(vec1 = as.numeric(seq(1, 2*10^8)),
                     vec2 = as.numeric(seq(1, 2*10^8)),
                     vec3 = as.numeric(seq(1, 2*10^8)))

pryr::object_size(DT3.3)
pryr::object_size(DT3.3b)
pryr::object_size(DT3.3c)
pryr::object_size(DT3.3d)
pryr::object_size(DT3.3e)
