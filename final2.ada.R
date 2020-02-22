library(caret)
library(doMC)

numCores <- detectCores()
registerDoMC(cores = numCores/2)

setB_last11 = read.csv("setB_last11.csv", header = TRUE)
setB_last10 = read.csv("setB_last10.csv", header = TRUE)
setB_last17 = read.csv("setB_last17.csv", header = TRUE)
# 
setA_last11 = read.csv("last11.csv", header = TRUE)
setA_last10 = read.csv("last10.csv", header = TRUE)
setA_last17 = read.csv("last17.csv", header = TRUE)
# 
setA = rbind(setA_last11, setA_last10, setA_last17)
setB = rbind(setB_last11, setB_last10, setB_last17)

# train vs test
validation <- createDataPartition(setB$SepsisLabel, p = 0.1, list = FALSE)
valid <- setB[validation,]
train <- setB[-validation,]

downSetB = downSample(x = train[, -42], y = as.factor(train$SepsisLabel))
names(downSetB)[44]<-"SepsisLabel"
table(downSetB$SepsisLabel)

train <- rbind(valid, downSetB)
test <- setA
table(train$SepsisLabel)

group1 = test[test$time > -10, ]
group2 = test[test$time < -9 & test$time > -16, ]
group3 = test[test$time < -15, ]

train[is.na(train)] <- 0
train_Y = ifelse(train$SepsisLabel==1, "Y", "N")
train = subset(train, select=-c(X, EtCO2, SepsisLabel, Patient, Bilirubin_direct, TroponinI, Fibrinogen))
train =data.frame(train, train_Y)

test[is.na(test)] <- 0
test_Y = ifelse(test$SepsisLabel==1, "Y", "N")
test = subset(test, select=-c(X, EtCO2, SepsisLabel, Patient, Bilirubin_direct, TroponinI, Fibrinogen))
test =data.frame(test, test_Y)

group1[is.na(group1)] <- 0
group1_Y = ifelse(group1$SepsisLabel==1, "Y", "N")
group1 = subset(group1, select=-c(X, EtCO2, SepsisLabel, Patient, Bilirubin_direct, TroponinI, Fibrinogen))
group1 = data.frame(group1, group1_Y)

group2[is.na(group2)] <- 0
group2_Y = ifelse(group2$SepsisLabel==1, "Y", "N")
group2 = subset(group2, select=-c(X, EtCO2, SepsisLabel, Patient, Bilirubin_direct, TroponinI, Fibrinogen))
group2 = data.frame(group2, group2_Y)

group3[is.na(group3)] <- 0
group3_Y = ifelse(group3$SepsisLabel==1, "Y", "N")
group3 = subset(group3, select=-c(X, EtCO2, SepsisLabel, Patient, Bilirubin_direct, TroponinI, Fibrinogen))
group3 = data.frame(group3, group3_Y)


predCorr <- cor(train[,-38])
highCorr <- findCorrelation(predCorr, .90)
train <- train[-highCorr]
test <- test[-highCorr]
group1 <- group1[-highCorr]
group2 <- group2[-highCorr]
group3 <- group3[-highCorr]


prePro_range <- preProcess(train, method = "range")
train <- predict(prePro_range, newdata = train)
test <- predict(prePro_range, newdata = test)
group1 <- predict(prePro_range, newdata = group1)
group2 <- predict(prePro_range, newdata = group2)
group3 <- predict(prePro_range, newdata = group3)

ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     index = list(TrainSet = -validation),
                     savePredictions = TRUE)

adaBoost <- train(x = train[,-37], 
                  y = train$train_Y,  
                  method = "adaboost", 
                  trControl = ctrl,
                  metric = "ROC", 
                  preProc = c("center", "scale"))



adaBoost

adaBoost$pred <- merge(adaBoost$pred,  adaBoost$bestTune)
adaBoostCM <- confusionMatrix(adaBoost, norm = "none")
adaBoostCM

adaBoostRoc <- roc(response = adaBoost$pred$obs,
                     predictor = adaBoost$pred$Y,
                     levels = rev(levels(adaBoost$pred$obs)))

update(plot(adaBoost, ylab = "ROC (Validation Data)"))
plot(adaBoostRoc, legacy.axes = TRUE)

adaImp <- varImp(adaBoost, scale = FALSE)

plot(adaImp, main = "Variable Importance with Ada Boost")

#########################
## This is real results
#########################

predicted = predict(adaBoost, test)
confusionMatrix(reference = test$test_Y, data = predicted, mode = "everything", positive = "Y")

predicted1 = predict(adaBoost, group1)

confusionMatrix(reference = group1$group1_Y, data = predicted1, mode = "everything", positive = "Y")

predicted2 = predict(adaBoost, group2)
confusionMatrix(reference = group2$group2_Y, data = predicted2, mode = "everything", positive = "Y")

predicted3 = predict(adaBoost, group3)
confusionMatrix(reference = group3$group3_Y, data = predicted3, mode = "everything", positive = "Y")

predicted_p = predict(adaBoost, newdata = test, type = "prob")
predicted1_p = predict(adaBoost, newdata = group1, type = "prob")
predicted2_p = predict(adaBoost, newdata = group2, type = "prob")
predicted3_p = predict(adaBoost, newdata = group3, type = "prob")

roc <- roc(response = test$test_Y,predictor = predicted_p$Y)
roc
roc1 <- roc(response = group1$group1_Y,predictor = predicted1_p$Y)
roc1
roc2 <- roc(response = group2$group2_Y,predictor = predicted2_p$Y)
roc2
roc3 <- roc(response = group3$group3_Y,predictor = predicted3_p$Y)
roc3





