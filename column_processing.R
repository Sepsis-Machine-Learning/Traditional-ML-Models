col.name = c("Glucose", "Lactate", "Magnesium", "Phosphate", "Potassium", "Bilirubin_total", "TroponinI", "Hct", "Hgb", "PTT", "WBC", "Fibrinogen", "Platelets", "Age", "Gender", "Unit1", "Unit2", "HospAdmTime", "ICULOS")
for (j in 1:length(col.name))
{
  dataset <- data.frame(read.csv(file_list[1], header = TRUE, sep = "|")[, c(col.name[j], "SepsisLabel")])
  dataset$Patient <- file_list[1]
  n = nrow(dataset)
  dataset$time = (-1 * (n - 1)):0
  names(dataset) <- c(" ", "  ", " ", " ")

  for (i in 2:length(file_list)) {
    temp_data <- data.frame(read.csv(file_list[i], header = TRUE, sep = "|")[, c(col.name[j], "SepsisLabel")]) #each file will be read in, specify which columns you need read in to avoid any errors
    temp_data$Patient <- file_list[i] #clean the data as needed, in this case I am creating a new column that indicates which file each row of data came from
    n = nrow(temp_data)
    temp_data$time = (-1 * (n - 1)):0
    names(temp_data) <- c(" ", "  ", " ", " ")
    dataset <- rbind(dataset, temp_data) #for each iteration, bind the new data to the building dataset
  }

  names(dataset) <- c(col.name[j], "SepsisLabel", "Patient", "Time")

  write.csv(dataset, paste(col.name[j], "csv", sep = "."))
  
  #alert user when the above methods are finished
  alarm()
}

rm("Alkalinephos", "AST", "Bilirubin_direct", "BUN", "Calcium", "Chloride", "Creatinine")

View(aa)
