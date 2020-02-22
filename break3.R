
#set the working directory from which the files will be read from
setwd("Downloads/training_setB")

#create a list of the files from your target directory
file_list <- list.files(path="Downloads/training")

#col.name = c("SaO2","AST","BUN","Alkalinephos","Calcium","Chloride","Creatinine","Bilirubin_direct", "Glucose", "Lactate", "Magnesium", "Phosphate", "Potassium", "Bilirubin_total", "TroponinI", "Hct", "Hgb", "PTT", "WBC", "Fibrinogen", "Platelets", "Age", "Gender", "Unit1", "Unit2", "HospAdmTime", "ICULOS")
# for (j in 1:length(col.name))
# {
  dataset <- data.frame(read.csv(file_list[1], header = TRUE, sep = "|"))
#  dataset <- data.frame(read.csv("p100055.psv", header = TRUE, sep = "|"))

  
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
#  names(dataset) <- c(" ", "  ", " ", " ")
  
  for (i in 2:length(file_list)) {
    temp_data <- data.frame(read.csv(file_list[i], header = TRUE, sep = "|")) #each file will be read in, specify which columns you need read in to avoid any errors
    temp_data$Patient <- file_list[i] #clean the data as needed, in this case I am creating a new column that indicates which file each row of data came from
    n = nrow(temp_data)
    temp_data$time = (-1 * (n - 1)):0
    last10 = subset(temp_data, time > -10)
    last11 = subset(temp_data, time < -9 & time > -16)
    if(n > 10) {
      final_diag = tail(temp_data, n=1)$SepsisLabel
      last11$SepsisLabel = final_diag
    }
    last17 = subset(temp_data, time < -15)
#    last10 = tail(temp_data, n =10)
    dataset1 <- rbind(dataset1, last10) #for each iteration, bind the new data to the building dataset
    dataset2 <- rbind(dataset2, last11) 
    dataset3 <- rbind(dataset3, last17) 
    }
  
#  names(dataset) <- c(col.name[j], "SepsisLabel", "Patient", "Time")
  
  write.csv(dataset1, "setB_last10.csv")
  write.csv(dataset2, "SetB_last11.csv")
  write.csv(dataset3, "SetB_last17.csv")
  
  
  #  Bilirubin_direct <- ggplot(dataset, aes(Time, Bilirubin_direct, group = Patient)) + geom_point() + geom_line(aes(color = SepsisLabel)) + labs(x = NULL, y = NULL) + scale_x_continuous(breaks = c(-300:0), labels = c(-300:0)) + coord_cartesian(xlim = c(-9, 0))
  
  #  saveRDS(Bilirubin_direct, file = "Bilirubin_direct.rds")
  
  #  print(Bilirubin_direct)
  
  alarm()



#names(dataset) <- c("HR", "SepsisLabel", "Patient", "Time")

#HR <- ggplot(dataset, aes(Time, HR, group = Patient)) + geom_point() + geom_line(aes(color = SepsisLabel)) + labs(x = NULL, y = NULL) + scale_x_continuous(breaks = c(-300:0), labels = c(-300:0)) + coord_cartesian(xlim = c(-9, 0))

col.name = c("HR", "Temp",	"SBP",	"MAP",	"DBP",	"Resp", "EtCO2",	"BaseExcess",	"HCO3",	"FiO2",	"pH",	"PaCO2", "O2Sat", "SaO2","AST","BUN","Alkalinephos","Calcium","Chloride","Creatinine","Bilirubin_direct", "Glucose", "Lactate", "Magnesium", "Phosphate", "Potassium", "Bilirubin_total", "TroponinI", "Hct", "Hgb", "PTT", "WBC", "Fibrinogen", "Platelets", "Age", "Gender", "ICULOS", "time")

for (j in 1:length(col.name)) {
  print(j)
  print(col.name[j])
  var=glm(SepsisLabel ~ col.name[j], data=last10 ,family=binomial)
  summary(var)
  
}

#GLM - Logistic regression
last10 = read.csv("last10.csv", header = TRUE)
last10 = last10[, c(2:37, 42,44)]
#last10$Gender=as.factor(last10$Gender)
#last10$SepsisLabel=as.factor(last10$SepsisLabel)
last10[is.na(last10)] = 0
dim(last10)
glm.fit = glm(SepsisLabel ~ ., data = last10, family = binomial)

last11 = read.csv("last11.csv", header = TRUE)
last11 = last11[, c(2:37, 42,44)]
#last11$Gender=as.factor(last11$Gender)
#last11$SepsisLabel=as.factor(last11$SepsisLabel)
last11[is.na(last11)] = 0
dim(last11)
cor(last11)
glm.fit = glm(SepsisLabel ~ ., data = last11, family = binomial)
summary(glm.fit)

glm.probs=predict(glm.fit,last10,type="response")
glm.probs=predict(glm.fit,last11,type="response")

glm.pred = rep(0, 114724)
glm.pred[glm.probs > 0.5] = 1
diag = last11$SepsisLabel
table(glm.pred,diag)

mean(glm.pred == diag)

#LDA
lda.fit=lda(SepsisLabel ~ ., data = last11)
summary(lda.fit)

lda.pred=predict(lda.fit, last10)
diag = last10$SepsisLabel

table(lda.pred$class ,diag)
mean(lda.pred$class == diag)

da.fit=lda(SepsisLabel ~ ., data = last10)
summary(lda.fit)

lda.pred=predict(lda.fit, last11)
diag = last11$SepsisLabel

table(lda.pred$class ,diag)
mean(lda.pred$class == diag)

#QDA

qda.fit=qda(SepsisLabel ~ ., data = last11)
qda.fit

qda.pred=predict(qda.fit, last10)
diag = last10$SepsisLabel

table(qda.pred$class ,diag)
mean(qda.pred$class == diag)

qda.fit=lda(SepsisLabel ~ ., data = last10)

qda.pred=predict(qda.fit, last11)
diag = last11$SepsisLabel

table(qda.pred$class ,diag)
mean(qda.pred$class == diag)


#KNN
last10 = read.csv("last10.csv", header = TRUE)
last10 = last10[, c(2:37, 42,44)]
#last11$Gender=as.factor(last11$Gender)
#last11$SepsisLabel=as.factor(last11$SepsisLabel)
last10[is.na(last10)] = 0

setB_last10 = read.csv("setB_last10.csv", header = TRUE)
setB_last10 = setB_last10[, c(2:37, 42,44)]
#last11$Gender=as.factor(last11$Gender)
#last11$SepsisLabel=as.factor(last11$SepsisLabel)
setB_last10[is.na(setB_last10)] = 0

#standarize

test.X = scale(last10[, c(-8,-37)])
train.X = scale(setB_last10[, c(-8,-37)])
test.Y = last10$SepsisLabel
train.Y = setB_last10$SepsisLabel

library(class)
set.seed (1)
knn.pred=knn(train.X,test.X,train.Y ,k=3)
table(knn.pred,test.Y)

