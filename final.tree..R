#set your working directory
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

set.seed(123)
mtryValues <- c(10, 12, 15)
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     index = list(TrainSet = -validation),
                     savePredictions = TRUE)

rfFit <- train(x = train[,-35], 
                   y = train$train_Y,
                   method = "rf",
                   ntree = 1000,
                   tuneGrid = data.frame(mtry = mtryValues),
                   importance = TRUE,
                   metric = "ROC",
                   trControl = ctrl)
rfFit

rfFit$pred <- merge(rfFit$pred,  rfFit$bestTune)
rfFitCM <- confusionMatrix(rfFit, norm = "none")
rfFitCM

rfFitRoc <- roc(response = rfFit$pred$obs,
                    predictor = rfFit$pred$Y,
                    levels = rev(levels(rfFit$pred$obs)))
update(plot(rfFit, ylab = "ROC (Validation Data)"))
plot(rfFitRoc, legacy.axes = TRUE)

rfImp <- varImp(rfFit, scale = FALSE)

rfImp
