setwd("Downloads/training_setB")
file_list <- list.files(path="Downloads/training")

#checking one file
dataset <- data.frame(read.csv(file_list[1], header = TRUE, sep = "|"))
dataset$Patient <- file_list[1]
n = nrow(dataset)
dataset$time = (-1 * (n - 1)):0
dataset1 = subset(dataset, time > -10)
dataset2 = subset(dataset, time < -9 & time > -16)
if(n > 10) {
  final_diag = tail(dataset, n=1)$SepsisLabel
  dataset2$SepsisLabel = final_diag
 }
dataset3 = subset(dataset, time < -15)

#first record previously processed above
for (i in 2:length(file_list)) {
  temp_data <- data.frame(read.csv(file_list[i], header = TRUE, sep = "|"))
  #cleaning data by creating a new column that indicates which file each row of data came from
  temp_data$Patient <- file_list[i] 
  #n=number of records for each given patient
  n = nrow(temp_data)
  #assign an hour number (counting backwards to final record) for each record for the given patient
  temp_data$time = (1-n):0
  #last10 = Clinical Confirmation Group (records -9 to 0)
  #last11 = Early Prediction Group (records -15 to -10)
  #last17 = Extraneous Group (records -16 and earlier)
  last10 = subset(temp_data, time > -10)
  last11 = subset(temp_data, time < -9 & time > -16)
  #if the Early Prediction Group exists for the given patient
  if(n > 10) {
    final_diag = tail(temp_data, n=1)$SepsisLabel
    last11$SepsisLabel = final_diag
  }
  last17 = subset(temp_data, time < -15)
  #bind current patient data to existing groups from previous iterations
  dataset1 <- rbind(dataset1, last10) 
  dataset2 <- rbind(dataset2, last11) 
  dataset3 <- rbind(dataset3, last17) 
  }
  
write.csv(dataset1, "setB_last10.csv")
write.csv(dataset2, "SetB_last11.csv")
write.csv(dataset3, "SetB_last17.csv")
