setwd("Downloads/Data_science")
library(caret)
library(doMC)
library(zoo)

numCores <- detectCores()
registerDoMC(cores = numCores-1)

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

#######################################
## Logistic Regression
#######################################
set.seed(123)
lrFit <- train(x = train[,-39],
                y = train$train_Y,
                method = "glm",
                metric = "ROC",
                trControl = ctrl)
lrFit


lrFit$pred <- merge(lrFit$pred,  lrFit$bestTune)

## Get the confusion matrices for the hold-out set
lrCM <- confusionMatrix(lrFit, norm = "none")
lrCM


## Get the area under the ROC curve for the hold-out set
lrRoc <- roc(response = lrFit$pred$obs,
             predictor = lrFit$pred$Y,
             levels = rev(levels(lrFit$pred$obs)))
lrRoc

lrImp <- varImp(lrFit, scale = FALSE)
lrImp

plot(lrImp, main = "Variable Importance with glm")

#################################################
## Linear Discriminant Analysis
################################################
set.seed(123)
ldaFit <- train(x = train[,-39],
                 y = train$train_Y,
                 method = "lda",
                 preProc = c("center","scale"),
                 metric = "ROC",
                 trControl = ctrl)
ldaFit



ldaFit$pred <- merge(ldaFit$pred,  ldaFit$bestTune)
ldaCM <- confusionMatrix(ldaFit, norm = "none")
ldaCM
ldaRoc <- roc(response = ldaFit$pred$obs,
              predictor = ldaFit$pred$Y,
              levels = rev(levels(ldaFit$pred$obs)))

ldaRoc

ldaImp <- varImp(ldaFit, scale = FALSE)
ldaImp

plot(ldaImp, main = "Variable Importance with lda")

################################################
## Partial Least Squares Discriminant Analysis
##################################################
set.seed(123)
plsFit <- train(x = train[, -39],
                 y = train$train_Y,
                 method = "pls",
                 tuneGrid = expand.grid(.ncomp = 1:10),
                 preProc = c("center","scale"),
                 metric = "ROC",
                 probMethod = "Bayes",
                 trControl = ctrl)

plsFit

plsImpFull <- varImp(plsFit, scale = FALSE)
plsImpFull

## Only keep the final tuning parameter data
plsFit$pred <- merge(plsFit$pred,  plsFull$bestTune)

plsRoc <- roc(response = plsFit$pred$obs,
              predictor = plsFit$pred$Y,
              levels = rev(levels(plsFit$pred$obs)))

plsRoc
### PLS confusion matrix information
plsCM <- confusionMatrix(plsFit, norm = "none")
plsCM




################################################################################
## Penalized Models - The glmnet model
################################################################################

glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1),
                        lambda = seq(.01, .2, length = 40))
set.seed(123)
glmnFit <- train(x = train[,-39], 
                 y = train$train_Y,
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = ctrl)
glmnFit

glmnFit$pred <- merge(glmnFit$pred,  glmnFit$bestTune)
glmnetCM <- confusionMatrix(glmnFit, norm = "none")
glmnetCM

glmnetRoc <- roc(response = glmnFit$pred$obs,
                 predictor = glmnFit$pred$successful,
                 levels = rev(levels(glmnFit$pred$obs)))

glmnetRoc

################################################################################
## Sparse logistic regression
################################################################################

set.seed(123)
spLDAFit <- train(x = train[,-39], 
                  y = train$train_Y,
                  method = "sparseLDA",
                  tuneGrid = expand.grid(lambda = c(.1),
                                         NumVars = c(1:20, 50, 75, 100, 250, 500, 750, 1000)),
                  preProc = c("center", "scale"),
                  metric = "ROC",
                  trControl = ctrl)
spLDAFit

spLDAFit$pred <- merge(spLDAFit$pred,  spLDAFit$bestTune)
spLDACM <- confusionMatrix(spLDAFit, norm = "none")
spLDACM

spLDARoc <- roc(response = spLDAFit$pred$obs,
                predictor = spLDAFit$pred$successful,
                levels = rev(levels(spLDAFit$pred$obs)))

spLDARoc

################################################################################
## Nearest Shrunken Centroids
################################################################################
set.seed(123)
nscFit <- train(x = train[,-39], 
                y = train$train_Y,
                method = "pam",
                preProc = c("center", "scale"),
                tuneGrid = data.frame(threshold = seq(0, 25, length = 30)),
                metric = "ROC",
                trControl = ctrl)
nscFit

nscFit$pred <- merge(nscFit$pred,  nscFit$bestTune)
nscCM <- confusionMatrix(nscFit, norm = "none")
nscCM
nscRoc <- roc(response = nscFit$pred$obs,
              predictor = nscFit$pred$successful,
              levels = rev(levels(nscFit$pred$obs)))
nscRoc
