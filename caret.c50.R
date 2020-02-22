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

c50Grid <- expand.grid(trials = c((1:10)*10),
                       model = c("tree", "rules"),
                       winnow = c(TRUE, FALSE))
set.seed(476)
c50Fit <- train(train, train$train_Y,
                    method = "C5.0",
                    tuneGrid = c50Grid,
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl)
c50Fit

c50Fit$pred <- merge(c50Fit$pred,  c50Fit$bestTune)
c50FitCM <- confusionMatrix(c50FitFit, norm = "none")
c50FitCM

c50FitRoc <- roc(response = c50Fit$pred$obs,
                     predictor = c50Fit$pred$Y,
                     levels = rev(levels(c50Fit$pred$obs)))

update(plot(c50Reduced), ylab = "ROC AUC (Validation Data)")
plot(c50FitRoc, legacy.axes = TRUE)

c50Imp <- varImp(c50Fit, scale = FALSE)

c50Imp


