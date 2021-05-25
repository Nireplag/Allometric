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