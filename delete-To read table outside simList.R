# To read outside the simList
typeDisturbance = c("Transitional", "Permanent", "Both", "Undisturbed")
disturbanceDimension = c("local", "neighborhood")
dataName = "Final_points_BEAD_final.csv"
source(file.path(getwd(), "modules","glmerBirdModels","R","dataUploading.R"))
dataset <- dataUploading(data = dataName, disturbanceDimension = disturbanceDimension, typeDisturbance = typeDisturbance)

