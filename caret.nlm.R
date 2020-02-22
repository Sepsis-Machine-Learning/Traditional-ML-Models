setwd("/Users/apple/Downloads/Data_science")
library(caret)
library(doMC)
library(zoo)

numCores <- detectCores()
registerDoMC(cores = numCores)

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
validation <- createDataPartition(setB$SepsisLabel, p = 0.2, list = FALSE)
train <- setB[-validation,]
valid <- setB[validation,]

train = downSample(x = train[, -42], y = as.factor(train$SepsisLabel))
names(train)[44]<-"SepsisLabel"
table(train$SepsisLabel)

train <- rbind(train, valid)
table(train$SepsisLabel)

test = setA

group1 = test[test$time > -10, ]
group2 = test[test$time < -9 & test$time > -16, ]
group3 = test[test$time < -15, ]

group2.patients = group2$Patient
group2.time = group2$time

train_patient = train$Patient
train_Y = ifelse(train$SepsisLabel==1, "Y", "N")
# train = subset(train, select=-c(X, EtCO2, SepsisLabel, Patient, Bilirubin_direct, TroponinI, Fibrinogen))
train = subset(train, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))
train = na.aggregate(train) 
train = data.frame(train, train_Y)
# keep <- Reduce('&', lapply(train, function(x) x >= quantile(x, .01) 
#                            & x <= quantile(x, .99)))
# train = train[keep,]

test_patient = test$Patient
test_Y = ifelse(test$SepsisLabel==1, "Y", "N")
test = subset(test, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))
test = na.aggregate(test) 
test =data.frame(test, test_Y)

group1_patient = group1$Patient
group1_Y = ifelse(group1$SepsisLabel==1, "Y", "N")
group1 = subset(group1, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))
group1 = na.aggregate(group1) 
group1 = data.frame(group1, group1_Y)

group2_patient = group2$Patient
group2_Y = ifelse(group2$SepsisLabel==1, "Y", "N")
group2 = subset(group2, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))
group2 = na.aggregate(group2) 
group2 = data.frame(group2, group2_Y)

group3_patient = group3$Patient
group3_Y = ifelse(group3$SepsisLabel==1, "Y", "N")
group3 = subset(group3, select=-c(X, EtCO2, SepsisLabel, Patient, TroponinI))
group3 = na.aggregate(group3) 
group3 = data.frame(group3, group3_Y)


predCorr <- cor(train[,-40])
highCorr <- findCorrelation(predCorr, .90)
train <- train[-highCorr]

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

################################################################################
## Nonlinear Discriminant Analysis
################################################################################

set.seed(123)
mdaFit <- train(train[,-39], 
                train$train_Y,
                    method = "mda",
                    metric = "ROC",
                    tries = 40,
                    tuneGrid = expand.grid(.subclasses = 1:8),
                    trControl = ctrl)

mdaFit

mdaFit$results <- mdaFit$results[!is.na(mdaFit$results$ROC),]                
mdaFit$pred <- merge(mdaFit$pred,  mdaFit$bestTune)
mdaCM <- confusionMatrix(mdaFit, norm = "none")
mdaCM

mdaRoc <- roc(response = mdaFit$pred$obs,
              predictor = mdaFit$pred$Y,
              levels = rev(levels(mdaFit$pred$obs)))
mdaRoc





################################################################################
## Flexible Discriminant Analysis
################################################################################

set.seed(123)
fdaFit <- train(x = train[,-39], 
                y = train$train_Y,
                method = "fda",
                metric = "ROC",
                tuneGrid = expand.grid(degree = 1, nprune = 2:25),
                trControl = ctrl)
fdaFit

fdaFit$pred <- merge(fdaFit$pred,  fdaFit$bestTune)
fdaCM <- confusionMatrix(fdaFit, norm = "none")
fdaCM

fdaRoc <- roc(response = fdaFit$pred$obs,
              predictor = fdaFit$pred$Y,
              levels = rev(levels(fdaFit$pred$obs)))


fdaRoc




################################################################################
## Naive Bayes
################################################################################



set.seed(476)
nBayesFit <- train(x = train[,reducedSet],
                   y = train$train_Y,
                   method = "nb",
                   metric = "ROC",
                   tuneGrid = data.frame(usekernel = c(TRUE, FALSE), fL = 2),
                   trControl = ctrl)
nBayesFit

nBayesFit$pred <- merge(nBayesFit$pred,  nBayesFit$bestTune)
nBayesCM <- confusionMatrix(nBayesFit, norm = "none")
nBayesCM
nBayesRoc <- roc(response = nBayesFit$pred$obs,
                 predictor = nBayesFit$pred$Y,
                 levels = rev(levels(nBayesFit$pred$obs)))
nBayesRoc




