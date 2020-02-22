
setwd("Downloads/data_science")
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

downSetB = downSample(x = setB[, -42], y = as.factor(setB$SepsisLabel))
names(downSetB)[44]<-"SepsisLabel"
table(downSetB$SepsisLabel)


# train vs test
validation <- createDataPartition(setA$SepsisLabel, p = 0.8, list = FALSE)

train <- setA[validation,]

test <- setA[-validation,]
train <- rbind(train, downSetB)
table(train$SepsisLabel)

library(skimr)
skimmed <- skim(train)
skimmed


train[is.na(train)] <- 0
train_Y = ifelse(train$SepsisLabel==1, "Y", "N")
train = subset(train, select=-c(X, EtCO2, SepsisLabel, Patient, Bilirubin_direct, TroponinI, Fibrinogen))
train =data.frame(train, train_Y)

test[is.na(test)] <- 0
test_Y = ifelse(test$SepsisLabel==1, "Y", "N")
test = subset(test, select=-c(X, EtCO2, SepsisLabel, Patient, Bilirubin_direct, TroponinI, Fibrinogen))
test =data.frame(test, test_Y)


predCorr <- cor(train[,-38])
highCorr <- findCorrelation(predCorr, .90)
train <- train[-highCorr]

prePro_range <- preProcess(train, method = "range")
train <- predict(prePro_range, newdata = train)

apply(train, 2, FUN = function(x) {c("min"=min(x), "max"=max(x))})

ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     index = list(TrainSet = -validation),
                     savePredictions = TRUE)

adaBoost <- train(x = train[,-35], 
                  y = train$train_Y,  
                  method = "adaboost", 
                  trControl = ctrl,
                  metric = "ROC", 
                  preProc = c("center", "scale"))

adaBoost

grid1 <- expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3),
                    coeflearn = c("Breiman", "Freund", "Zhu"))

adaBoostM1 <- train(x = train[,-35], 
                    y = train$train_Y, 
                    method = "AdaBoost.M1", 
                    trControl = ctrl,
                    tuneGrid = grid1,
                    metric = "ROC", 
                    preProc = c("center", "scale"))


grid2 <- expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3))

adaBag <- train(x = train[,-35], 
                y = train$train_Y,  
                method = "AdaBag", 
                trControl = ctrl,
                tuneGrid = grid2,
                metric = "ROC", 
                preProc = c("center", "scale"))





gbmFit

gbmFit$pred <- merge(gbmFit$pred,  gbmFit$bestTune)
gbmFitCM <- confusionMatrix(gbmFit, norm = "none")
gbmFitCM

gbmFitRoc <- roc(response = gbmFit$pred$obs,
                     predictor = gbmFit$pred$Y,
                     levels = rev(levels(gbmFit$pred$obs)))

update(plot(gbmFit, ylab = "ROC (Validation Data)"))
plot(gbmFitRoc, legacy.axes = TRUE)

gbmImp <- varImp(gbmFit, scale = FALSE)

gbmImp

