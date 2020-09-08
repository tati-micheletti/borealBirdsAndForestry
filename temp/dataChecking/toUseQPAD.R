# To use QPAD

reproducible::Require("detect")
reproducible::Require("QPAD")
load_BAM_QPAD(version = 2)

oven <- detect::oven
oven$JDAY <- oven$julian/365
oven$TSSR <- ((oven$timeday/8) - 0.75)/24
oven$xlat <- as.numeric(scale(oven$lat)) # latitude is standardized
oven$xlong <- as.numeric(scale(oven$long)) # longitude is standardized
oven$dur <- 3
oven$dist <- Inf
pf <- oven$pforest
pd <- oven$pdecid
pc <- pf - pd
oven$LCC <- factor(5, levels = 1:5) # 5=OH open habitat
oven$LCC[pf > 0.25 & pc > pd] <- "3" # 3=SC dense conifer
oven$LCC[pf > 0.25 & pc <= pd] <- "4" # 4=SD sparse deciduous
oven$LCC[pf > 0.6 & pc > pd] <- "1" # 1=DC dense conifer
oven$LCC[pf > 0.6 & pc <= pd] <- "2" # 2=DC dense deciduous
bm <- bestmodelBAMspecies("OVEN", type = "BIC")
bc <- with(oven, localBAMcorrections("OVEN", t = dur, r = dist,
                                     jday = JDAY, tssr = TSSR, tree = pforest, lcc = LCC, model.sra = bm$sra,
                                     model.edr = bm$edr))

mod <- glm(formula = count ~ pforest + xlong, 
           data = oven, 
           family = poisson(link = "log"),
            offset = corrections2offset(bc))
