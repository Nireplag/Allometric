# load packages
library(caret)
library(neuralnet)

# load csv database
Volumes <- read.csv2("http://www.razer.net.br/datasets/Volumes.csv")

# Eliminate column NR
Volumes <- Volumes[ , -1]

# split dataset into two 
set.seed(9)
index <- createDataPartition(Volumes$VOL, p=0.80, list = FALSE) 

volume_train <- Volumes[index, ]
volume_test <- Volumes[-index, ]

# train random Forest model
rf <- train(VOL~., data = volume_train, method = "rf") # train model
saveRDS(rf, "rf.rds") # save trained model

# train SVM model
svm <- train(VOL~., data = volume_train, method = "svmRadial")
saveRDS(svm, "svm.rds") #save trained model

# train neural networks
nn <- train(VOL~., data = volume_train, method = "neuralnet")
saveRDS(nn, "neuralnet.rds") #save trained model

# alometric model 
alom <- nls(VOL ~ b0 + b1 * DAP * DAP * HT, volume_train, start = list(b0 = 0.5, b1 = 0.5))
saveRDS(alom, "allometric.rds")
# predictions of test data

rf <- readRDS("rf.rds")
svm <- readRDS("svm.rds")
nn <- readRDS("neuralnet.rds")
alom <- readRDS("allometric.rds")
predictions.rf <- predict(rf, volume_test) # predict for random forest
predictions.svm <- predict(svm, volume_test) # predict for SVM
predictions.nn <- predict(nn, volume_test) # predict for neuralnet
predictions.alom <- predict(alom, volume_test) # predict for alometric model

# define determination coefficient: r2

r2 <- function(obs, pred){
  obs_mean <- mean(obs)
  num <- sum((obs - pred)^2)
  den <- sum((obs - obs_mean)^2)
  result <- 1 - (num/den)
  return(result)
}

# define estimation standard error : syx

syx <- function(obs, pred){
  num <- sum((obs - pred)^2)
  den <- length(obs) - 2
  result <- sqrt(num/den)
  return(result)
}

Syx_100 <- function(syx, obs){
  result <- 100* syx/mean(obs)
  return(result)
}

# prediction of metrics
name <- c("R2", "Syx", "Syx%") 

# Random forest
metrics.rf <- c(r2(volume_test$VOL, predictions.rf), syx(volume_test$VOL, predictions.rf), Syx_100(syx(volume_test$VOL, predictions.rf), volume_test$VOL))
names(metrics.rf) <- name

# SVM
metrics.svm <- c(r2(volume_test$VOL, predictions.svm), syx(volume_test$VOL, predictions.svm), Syx_100(syx(volume_test$VOL, predictions.svm), volume_test$VOL))
names(metrics.svm) <- name

# neuralnet
metrics.nn <- c(r2(volume_test$VOL, predictions.nn), syx(volume_test$VOL, predictions.nn), Syx_100(syx(volume_test$VOL, predictions.nn), volume_test$VOL))
names(metrics.nn) <- name

# Alom
metrics.alom <- c(r2(volume_test$VOL, predictions.alom), syx(volume_test$VOL, predictions.alom), Syx_100(syx(volume_test$VOL, predictions.alom), volume_test$VOL))
names(metrics.alom) <- name

metrics <- data.frame(metrics.rf, metrics.svm, metrics.nn, metrics.alom)

sink("analysis.txt") # output file for decision upon best model
metrics
sink()


