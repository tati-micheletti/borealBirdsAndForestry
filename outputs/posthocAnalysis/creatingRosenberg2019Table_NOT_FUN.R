library("data.table")
library("reproducible")
library("usefun")
popChangeAbund <- prepInputs(url = "https://drive.google.com/open?id=1CnejLh_83OzXVKf5HORskA9dnqCohKSJ", 
                             destinationPath = tempdir(), targetFile = "popChangeBySpeciesRosenberg_2019.csv",
                             fun = "data.table::fread")
popChangeMySpecies <- prepInputs(url = "https://drive.google.com/open?id=1ww9esByKVc1KC5kYwTzElNnSaHE1ksTq", 
                                 destinationPath = tempdir(), targetFile = "trends.csv",
                                 fun = "data.table::fread")
names(popChangeMySpecies)[names(popChangeMySpecies) == "Scientific name"] <- "sci_name"
popChangeRed <- popChangeAbund[sci_name %in% popChangeMySpecies$sci_name, ]
popChangeRed <- merge(popChangeRed, popChangeMySpecies, by = "sci_name")
popChangeRedSimp <- popChangeRed[, c("Four Letter Code", "popest", "popestlci", "popestuci",
                                     "Loss_med", "Loss_lci", "Loss_uci","Trajectory_firstyear","Trajectory_lastyear")] #"Breeding.Biome", "sci_name", "Preferred.habitat.b",  "lowerUSATrend", "upperUSATrend", "lowerCATrend", "upperCATrend", 
popChangeRedSimp[, c("lossPerc", "lossLoPerc", "lossUpPerc") := list(Loss_med/(popest+Loss_med), Loss_lci/(popestuci+Loss_lci), Loss_uci/(popestlci+Loss_uci))]
popChangeRedSimp[, c("lossPercYear", "lossLoPercYear", "lossUpPercYear") :=  list(
  lossPerc/(Trajectory_lastyear-Trajectory_firstyear), 
  lossLoPerc/(Trajectory_lastyear-Trajectory_firstyear), 
  lossUpPerc/(Trajectory_lastyear-Trajectory_firstyear))]
popChangeRedSimp[, c("Trajectory_firstyear","Trajectory_lastyear") := NULL]
write.csv(x = popChangeRedSimp, file = file.path(tempdir(), "popChangeRosenberg2019.csv"))
id <- "12iUWhrSBDS8dDXI42uSFqns_7akFSr1R"
fl <- usefun::grepMulti(x = list.files(tempdir(),
                                       full.names=T), patterns = "popChangeRosenberg2019")
lapply(fl, function(ras){
  googledrive::drive_upload(media = ras, path = googledrive::as_id(id))
})
