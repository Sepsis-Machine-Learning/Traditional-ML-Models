setwd("Downloads/Data_science")
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

group2.patients = group2$Patient
group2.time = group2$time

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

c50Grid <- expand.grid(trials = c((1:10)*10),
                       model = c("tree", "rules"),
                       winnow = c(TRUE, FALSE))
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     index = list(TrainSet = -validation),
                     savePredictions = TRUE)
set.seed(476)
c50Fit <- train(train[,-37], train$train_Y,
                    method = "C5.0",
                    tuneGrid = c50Grid,
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl)
c50Fit

c50Fit$pred <- merge(c50Fit$pred,  c50Fit$bestTune)
c50FitCM <- confusionMatrix(c50Fit, norm = "none")
c50FitCM

c50FitRoc <- roc(response = c50Fit$pred$obs,
                 predictor = c50Fit$pred$Y,
                 levels = rev(levels(c50Fit$pred$obs)))

update(plot(c50Fit), ylab = "ROC AUC (Validation Data)")
plot(c50FitRoc, legacy.axes = TRUE)

c50Imp <- varImp(c50Fit, scale = FALSE)


plot(c50Imp, main = "Variable Importance with C5.0")

#########################
## This is real results
#########################

predicted = predict(c50Fit, test)
confusionMatrix(reference = test$test_Y, data = predicted, mode = "everything", positive = "Y")

predicted1 = predict(c50Fit, group1)

confusionMatrix(reference = group1$group1_Y, data = predicted1, mode = "everything", positive = "Y")

predicted2 = predict(c50Fit, group2)
confusionMatrix(reference = group2$group2_Y, data = predicted2, mode = "everything", positive = "Y")

library("dplyr")
form = data.frame(predicted2, group2.patients, group2.time, group2_Y)
flag1 = (form$predicted2=='Y' & form$group2_Y == "Y")
correct = form[flag1,]


t6 = correct[correct$group2.time==-15,]

t5 = correct[correct$group2.time==-14,]
t5 = t5[!(t5$group2.patients %in% t6$group2.patients),]

t4 = correct[correct$group2.time==-13,]
t4 = t4[!(t4$group2.patients %in% t6$group2.patients),]
t4 = t4[!(t4$group2.patients %in% t5$group2.patients),]

t3 = correct[correct$group2.time==-12,]
t3 = t3[!(t3$group2.patients %in% t6$group2.patients),]
t3 = t3[!(t3$group2.patients %in% t5$group2.patients),]
t3 = t3[!(t3$group2.patients %in% t4$group2.patients),]

t2 = correct[correct$group2.time==-11,]
t2 = t2[!(t2$group2.patients %in% t6$group2.patients),]
t2 = t2[!(t2$group2.patients %in% t5$group2.patients),]
t2 = t2[!(t2$group2.patients %in% t4$group2.patients),]
t2 = t2[!(t2$group2.patients %in% t3$group2.patients),]

t1 = correct[correct$group2.time==-10,]
t1 = t1[!(t1$group2.patients %in% t6$group2.patients),]
t1 = t1[!(t1$group2.patients %in% t5$group2.patients),]
t1 = t1[!(t1$group2.patients %in% t4$group2.patients),]
t1 = t1[!(t1$group2.patients %in% t3$group2.patients),]
t1 = t1[!(t1$group2.patients %in% t2$group2.patients),]

flag2 = form$predicted2=='N' & form$group2_Y == "Y"

wrong = form[flag2,]

wrong %>% group_by(group2.patients) %>% summarize(count = n())

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


