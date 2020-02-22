#set your working directory
setwd("......")
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


xgbGrid1 <- expand.grid(nrounds = c(1, 10),
                       max_depth = c(1, 2, 4),
                       eta = c(.1, .3),
                       gamma = c(0, 0.1),
                       colsample_bytree = c(.7, .9),
                       min_child_weight = c(1, 2),
                       subsample = c(.75, 1))
xgbGrid2 <- expand.grid(nrounds = c(1, 10), 
                       max_depth = c(1, 2, 4),
                       eta = c(.1, .3),
                       rate_drop = 0.10,
                       skip_drop = 0.10,
                       colsample_bytree = c(.7, .9),
                       min_child_weight = c(1, 2),
                       subsample = c(.75, 1),
                       gamma = c(0, 0.1))


xgbTreeFit <- train(x = train[,-35], 
                    y = train$train_Y,
                    method = "xgbTree", 
                    trControl = ctrl,
                    metric = "ROC", 
                    preProc = c("center", "scale"),
                    tuneGrid = xgbGrid1)




xgbTreeFit

xgbTreeFit$pred <- merge(xgbTreeFit$pred,  xgbTreeFit$bestTune)
xgbTreeFitCM <- confusionMatrix(xgbTreeFit, norm = "none")
xgbTreeFitCM

xgbTreeFitRoc <- roc(response = xgbTreeFit$pred$obs,
                     predictor = xgbTreeFit$pred$Y,
                     levels = rev(levels(xgbTreeFit$pred$obs)))

update(plot(xgbTreeFit, ylab = "ROC (Validation Data)"))
plot(xgbTreeFitRoc, legacy.axes = TRUE)

xgbTreeImp <- varImp(xgbTreeFit, scale = FALSE)

xgbTreeImp

xgbDARTFit <- train(trainX, trainY, 
                             method = "xgbDART", 
                             trControl = ctrl,
                             metric = "ROC", 
                             preProc = c("center", "scale"),
                             tuneGrid = xgbGrid2)

xgbDARTFit

xgbDARTFit$pred <- merge(xgbDARTFit$pred,  xgbDARTFit$bestTune)
xgbDARTFitCM <- confusionMatrix(xgbDARTFit, norm = "none")
xgbDARTFitCM

xgbDARTFitRoc <- roc(response = xgbDARTFit$pred$obs,
                     predictor = xgbDARTFit$pred$Y,
                     levels = rev(levels(xgbDARTFit$pred$obs)))

update(plot(xgbDARTFit, ylab = "ROC (Validation Data)"))
plot(xgbDARTFitRoc, legacy.axes = TRUE)

xgbDARTImp <- varImp(xgbDARTFit, scale = FALSE)

xgbDARTImp


