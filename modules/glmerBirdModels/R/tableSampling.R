

tableSampling <- function(sim = sim, dataName = sim$dataName, dataset = sim$data){
  
  data.path <- file.path(getwd(), "modules/glmerBirdModels/data", dataName)
  fullData <- suppressWarnings(fread(data.path))
  
  fullData <- fullData[order(YYYY)]
  Year <- fullData[,.(Year=length(unique(ClusterSP))), by = YYYY]
  Surveys <- fullData[,.(Surveys=sum(.N)), by = YYYY]
  L_Disturbed <- fullData[!(State_P_100==0),.(L_Disturbed=sum(.N)), by = YYYY]
  L_Undisturbed <- fullData[State_P_100==0,.(L_Undisturbed=sum(.N)), by = YYYY]
  N_Disturbed <- fullData[!(State_P_500==0),.(N_Disturbed=sum(.N)), by = YYYY]
  N_Undisturbed <- fullData[State_P_500==0,.(N_Undisturbed=sum(.N)), by = YYYY]
  
  tableS2 <- Reduce(function(...) merge(..., all = TRUE, by = "YYYY"), 
                    list(Year, Surveys, L_Disturbed, L_Undisturbed, N_Disturbed, N_Undisturbed))
  
  write.csv(tableS2, file.path(outputPath(sim), "TableS2.csv"))
  
  return(tableS2)
  
}