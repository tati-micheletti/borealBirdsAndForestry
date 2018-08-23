A <- runif(n = 1000, min = 9, max = 10)
B <- runif(n = 1000, min = 8, max = 13)
mod <- glm(formula = A ~ B)
rasA <- raster::raster(x = as.matrix(A))
rasB <- raster::raster(x = as.matrix(B))
ras1 <- raster::stack(rasA, rasB)
names(ras1) <- c("A", "B")
prd <- raster::predict(ras1, mod)

# Testing predicting with NA
rasA[500:700] <- NA
ras2 <- raster::stack(rasA, rasB)
names(ras2) <- c("A", "B")
prd <- raster::predict(ras2, mod)
