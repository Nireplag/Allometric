# load packages
library(caret)

# load csv database
Volumes <- read.csv2("http://www.razer.net.br/datasets/Volumes.csv")

# Eliminate column NR
Volumes <- Volumes[ , -1]

# split dataset into two 
set.seed(9)
index <- createDataPartition(Volumes$VOL, p=0.80, list = FALSE) 

volume_train <- Volumes[index, ]
volume_test <- Volumes[-index, ]