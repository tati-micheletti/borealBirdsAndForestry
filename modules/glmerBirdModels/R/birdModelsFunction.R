birdModelsFunction <- function(sim, disturbanceDimension, typeDisturbance){
  
# disturbanceDimension <- c("local", "neighborhood)
# typeDisturbance <- c("Transitional", "Permanent", "Undisturbed", "Both")
#  For undisturbed, it uses neighborhood model, with local==0

  require(lme4)
  
  models <- list()
  
  for (dd in 1:length(disturbanceDimension)){
    for (td in 1:length(typeDisturbance)){
    
      
  if (disturbanceDimension[dd]=="local"){
    dimension <- paste0("State_P_100")
  }
    
  if (disturbanceDimension[dd]=="neighborhood"){
    dimension <- paste0("State_P_500")
  }

    if (typeDisturbance[td]=="Both"){

      data <- sim$data$fullData
  
      }  else
  
      if (disturbanceDimension[dd]=="neighborhood"&typeDisturbance[td]=="Undisturbed"){
        
        next

        } else {
        
      data <- sim$data[[paste0(disturbanceDimension[dd],typeDisturbance[td])]]
      
      }
    
  ##### MODELS #####
  
  BBWA <- glmer(AB_BBWA ~ get(dimension) + LOG_BCR_BBWA + offset(OF_BBWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  BLPW <- glmer(AB_BLPW ~ get(dimension) + LOG_BCR_BLPW + offset(OF_BLPW) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  BOCH <- glmer(AB_BOCH ~ get(dimension) + LOG_BCR_BOCH + offset(OF_BOCH) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  BRCR <- glmer(AB_BRCR ~ get(dimension) + LOG_BCR_BRCR + offset(OF_BRCR) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  BTNW <- glmer(AB_BTNW ~ get(dimension) + LOG_BCR_BTNW + offset(OF_BTNW) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  CAWA <- glmer(AB_CAWA ~ get(dimension) + LOG_BCR_CAWA + offset(OF_CAWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  CMWA <- glmer(AB_CMWA ~ get(dimension) + LOG_BCR_CMWA + offset(OF_CMWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  CONW <- glmer(AB_CONW ~ get(dimension) + LOG_BCR_CONW + offset(OF_CONW) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  FOSP <- glmer(AB_FOSP ~ get(dimension) + LOG_BCR_FOSP + offset(OF_FOSP) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  NOWA <- glmer(AB_NOWA ~ get(dimension) + LOG_BCR_NOWA + offset(OF_NOWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  OVEN <- glmer(AB_OVEN ~ get(dimension) + LOG_BCR_OVEN + offset(OF_OVEN) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  PISI <- glmer(AB_PISI ~ get(dimension) + LOG_BCR_PISI + offset(OF_PISI) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  RBNU <- glmer(AB_RBNU ~ get(dimension) + LOG_BCR_RBNU + offset(OF_RBNU) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  SWTH <- glmer(AB_SWTH ~ get(dimension) + LOG_BCR_SWTH + offset(OF_SWTH) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  TEWA <- glmer(AB_TEWA ~ get(dimension) + LOG_BCR_TEWA + offset(OF_TEWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  WETA <- glmer(AB_WETA ~ get(dimension) + LOG_BCR_WETA + offset(OF_WETA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  YRWA <- glmer(AB_YRWA ~ get(dimension) + LOG_BCR_YRWA + offset(OF_YRWA) + (1|ClusterSP) + (1|YYYY) + (1|ClusterSP:YYYY), family="poisson", data=data)
  
models[[paste0(disturbanceDimension[dd],typeDisturbance[td])]] <- list(BBWA = BBWA,
                                                            BLPW = BLPW,
                                                            BOCH = BOCH,
                                                            BRCR = BRCR,
                                                            BTNW = BTNW,
                                                            CAWA = CAWA,
                                                            CMWA = CMWA,
                                                            CONW = CONW,
                                                            FOSP = FOSP,
                                                            NOWA = NOWA,
                                                            OVEN = OVEN,
                                                            PISI = PISI,
                                                            RBNU = RBNU,
                                                            SWTH = SWTH,
                                                            TEWA = TEWA,
                                                            WETA = WETA,
                                                            YRWA = YRWA)
    }
  }
  
  return(models)
}