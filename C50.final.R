setwd("Downloads/Data_science")
library(caret)
library(doMC)

numCores <- detectCores()
registerDoMC(cores = numCores-2)

#last10 = Clinical Confirmation Group
#last11 = Early Prediction Group
#last17 = Extraneous Group
setB_last10 = read.csv("setB_last10.csv", header = TRUE)
setB_last11 = read.csv("setB_last11.csv", header = TRUE)
setB_last17 = read.csv("setB_last17.csv", header = TRUE)

setA_last10 = read.csv("last10.csv", header = TRUE)
setA_last11 = read.csv("last11.csv", header = TRUE)
setA_last17 = read.csv("last17.csv", header = TRUE)

#setA for testing, setB for training/cross-validation
setA = rbind(setA_last11, setA_last10, setA_last17)
setB = rbind(setB_last11, setB_last10, setB_last17)

# training vs validation
validation <- createDataPartition(setB$SepsisLabel, p = 0.2, list = FALSE)
train <- setB[-validation,]
valid <- setB[validation,]

#downsampling to balance training set classes, 42 is patient ID, 44 is sepsis label
train = downSample(x = train[, -42], y = as.factor(train$SepsisLabel))
names(train)[44]<-"SepsisLabel"
train <- rbind(train, valid)
table(train$SepsisLabel)

#testing
test = setA

#group1 = Clinical Confirmation Group
#group2 = Early Prediction Group
#group3 = Extraneous Group
group1 = test[test$time > -10, ]
group2 = test[test$time < -9 & test$time > -16, ]
group3 = test[test$time < -15, ]

group2.patients = group2$Patient
group2.time = group2$time

train_patient = train$Patient
train_Y = ifelse(train$SepsisLabel==1, "Y", "N")
#remove extraneous variables: X is the row number of orignal data, EtCO2 is missing from testing data (Set A)
#also remove Sepsis Label because it is our result, Patient ID is not useful, 99.9% of TroponinI is None
train = subset(train, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))

#replacing NaN with mean value - we found this was best compared to replacing with 0 or using Amelia package
train = na.aggregate(train) 
train = data.frame(train, train_Y)

test_patient = test$Patient
test_Y = ifelse(test$SepsisLabel==1, "Y", "N")
test = subset(test, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))
#replacing NaN with mean value
test = na.aggregate(test) 
test = data.frame(test, test_Y)

group1_patient = group1$Patient
group1_Y = ifelse(group1$SepsisLabel==1, "Y", "N")
group1 = subset(group1, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))
#replacing NaN with mean value
group1 = na.aggregate(group1) 
group1 = data.frame(group1, group1_Y)

group2_patient = group2$Patient
group2_Y = ifelse(group2$SepsisLabel==1, "Y", "N")
group2 = subset(group2, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))
#replacing NaN with mean value
group2 = na.aggregate(group2) 
group2 = data.frame(group2, group2_Y)

group3_patient = group3$Patient
group3_Y = ifelse(group3$SepsisLabel==1, "Y", "N")
group3 = subset(group3, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))
#replacing NaN with mean value
group3 = na.aggregate(group3) 
group3 = data.frame(group3, group3_Y)

#remove highly correlated variables (ignoring SepsisLabel)
predCorr <- cor(train[,-40])
highCorr <- findCorrelation(predCorr, .90)
train <- train[-highCorr]

#pre-processing: range of all variables from 0 to 1
prePro_range <- preProcess(train, method = "range")
train <- predict(prePro_range, newdata = train)
test <- predict(prePro_range, newdata = test)
group1 <- predict(prePro_range, newdata = group1)
group2 <- predict(prePro_range, newdata = group2)
group3 <- predict(prePro_range, newdata = group3)

#use Monte Carlo Cross Validation and display two-class summary
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     index = list(TrainSet = -validation),
                     savePredictions = TRUE)


################################################################################
## model training
################################################################################

set.seed(123)

c50Grid <- expand.grid(trials = c((1:10)*10),
                       model = c("tree", "rules"),
                       winnow = c(TRUE, FALSE))

#39 is sepsis label
#train C5.0 model
c50Fit <- train(train[,-39], train$train_Y,
                method = "C5.0",
                tuneGrid = c50Grid,
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl)
c50Fit

#calculate confusion matrix for best model
c50Fit$pred <- merge(c50Fit$pred,  c50Fit$bestTune)
c50FitCM <- confusionMatrix(c50Fit, norm = "none")
c50FitCM

#ROC of best model
c50FitRoc <- roc(response = c50Fit$pred$obs,
                 predictor = c50Fit$pred$Y,
                 levels = rev(levels(c50Fit$pred$obs)))
update(plot(c50Fit), ylab = "ROC AUC (Validation Data)")
plot(c50FitRoc, legacy.axes = TRUE)

#most important variables for this model
c50Imp <- varImp(c50Fit, scale = FALSE)
c50Imp

plot(c50Imp, main = "Variable Importance with C5.0")

#########################
## Real Predictions
#########################

predicted = predict(c50Fit, test)
confusionMatrix(reference = test$test_Y, data = predicted, mode = "everything", positive = "Y")

#group 1
predicted1 = predict(c50Fit, group1)
confusionMatrix(reference = group1$group1_Y, data = predicted1, mode = "everything", positive = "Y")

#group 2
predicted2 = predict(c50Fit, group2)
confusionMatrix(reference = group2$group2_Y, data = predicted2, mode = "everything", positive = "Y")

# Quantify Prediction Accuracy Among Sepsis Patients 
library("dplyr")
form = data.frame(predicted2, group2.patients, group2.time, group2_Y)
#true positive
flag1 = (form$predicted2=='Y' & form$group2_Y == "Y")
correct = form[flag1,]

#patients - 6hrs early - correct prediction. This represents the # of correctly predicted patients (not just records)
#because for a given hour, each patient has exactly one record.
t6 = correct[correct$group2.time==-15,]

#patients - 5hrs early - correct prediction. Not including any patients who were previously already predicted to have sepsis.
t5 = correct[correct$group2.time==-14,]
t5 = t5[!(t5$group2.patients %in% t6$group2.patients),]

#patients - 4hrs early - correct prediction
t4 = correct[correct$group2.time==-13,]
t4 = t4[!(t4$group2.patients %in% t6$group2.patients),]
t4 = t4[!(t4$group2.patients %in% t5$group2.patients),]

#patients - 3hrs early - correct prediction
t3 = correct[correct$group2.time==-12,]
t3 = t3[!(t3$group2.patients %in% t6$group2.patients),]
t3 = t3[!(t3$group2.patients %in% t5$group2.patients),]
t3 = t3[!(t3$group2.patients %in% t4$group2.patients),]

#patients - 2hrs early - correct prediction
t2 = correct[correct$group2.time==-11,]
t2 = t2[!(t2$group2.patients %in% t6$group2.patients),]
t2 = t2[!(t2$group2.patients %in% t5$group2.patients),]
t2 = t2[!(t2$group2.patients %in% t4$group2.patients),]
t2 = t2[!(t2$group2.patients %in% t3$group2.patients),]

#patients - 1hr early - correct prediction
t1 = correct[correct$group2.time==-10,]
t1 = t1[!(t1$group2.patients %in% t6$group2.patients),]
t1 = t1[!(t1$group2.patients %in% t5$group2.patients),]
t1 = t1[!(t1$group2.patients %in% t4$group2.patients),]
t1 = t1[!(t1$group2.patients %in% t3$group2.patients),]
t1 = t1[!(t1$group2.patients %in% t2$group2.patients),]

#false negative
flag2 = form$predicted2=='N' & form$group2_Y == "Y"

wrong = form[flag2,]

#patients - incorrect prediction
incorrect <- wrong %>% group_by(group2.patients) %>% summarize(count = n())

#group 3
predicted3 = predict(c50Fit, group3)
confusionMatrix(reference = group3$group3_Y, data = predicted3, mode = "everything", positive = "Y")

predicted_p = predict(c50Fit, newdata = test, type = "prob")
predicted1_p = predict(c50Fit, newdata = group1, type = "prob")
predicted2_p = predict(c50Fit, newdata = group2, type = "prob")
predicted3_p = predict(c50Fit, newdata = group3, type = "prob")

roc <- roc(response = test$test_Y,predictor = predicted_p$Y)
roc
roc1 <- roc(response = group1$group1_Y,predictor = predicted1_p$Y)
roc1
roc2 <- roc(response = group2$group2_Y,predictor = predicted2_p$Y)
roc2
roc3 <- roc(response = group3$group3_Y,predictor = predicted3_p$Y)
roc3
