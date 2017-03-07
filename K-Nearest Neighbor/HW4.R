library(ISLR)
install.packages("ISLR")
install.packages("MASS")
install.packages("dummies")
library(dummies)
library(ISLR)
library(MASS)
install.packages("caret")
install.packages("readx1")
install.packages("readxl")
library(readxl)
library(caret)
library(lattice)
library(ggplot2)
install.packages("ROCR")
library(ROCR)
library(gplots)
library(kknn)
library(class)
library(e1071)
setwd("~/Documents/Data mining*")
Creditdata1 <- read_excel("credit.xlsx",1)
Creditdata1$Profitable <- ifelse(Creditdata1$NPV> 0, 1, 0)
Creditdata1$Profitable<- as.numeric(as.character(Creditdata1$Profitable))
Creditdata1 <- dummy.data.frame(Creditdata1, c("CHK_ACCT", "SAV_ACCT", "HISTORY", "JOB", "TYPE",  "NUM_CREDITS"))
Creditdata1$`OBS#` <- NULL
Creditdata1$CREDIT_EXTENDED <- NULL
Creditdata1$NPV <- NULL

####
set.seed(1234)
Creditdata1[,-c(44)] <- scale(Creditdata1[,-c(44)])
N <- length(Creditdata1$Profitable)
train <- sort(sample(N, 0.7*N))
validate <- seq(N)[-train]
train_input <- Creditdata1[train,-44]
validate_input <- Creditdata1[validate,-44]
train_output <- Creditdata1[train,44]
validation_output <- Creditdata1[validate,44]

#2

error_train_list = seq(15)
error_validate_list = seq(15)
for(i in seq(15)){
  prediction_train = knn(train_input, train_input, train_output, k=i)
  prediction_validate = knn(train_input, validate_input, train_output, k=i)
  error_train_list[i] = mean(abs(as.numeric(as.character(prediction_train))-train_output))
  error_validate_list[i] = mean(abs(as.numeric(as.character(prediction_validate))-validation_output))
} 

##EXHIBIT 4
plot(c(1,15),c(0,0.5), type = "n", xlab ="k values", ylab="Error Rate")
lines(error_train_list, col="blue", type = 'l')  
lines(error_validate_list, col= "red", type = 'l')

#3a briefly explain why the % errro is zer for the training sample when k =1, but not for the validation sample
#The % Error is 0 for training sample since, k=1 is simply the nearest neighbor algorithm and hence no numbers of 
#neighbors influences the classification. k=1 minimizes the classification error on validation data but it isn’t 0 
since it is not the same as the training sample with which we are comparing for the nearest neighbors. 

min.error.validation <- which.min(error_validate_list)
min.error.validation

##explain
knn_predict <- knn(train_input, validate_input, train_output, k= 13)


table(knn_predict, validation_output, dnn = c("Prediction", "Actual") )
##4
Error <- (19+58)/ (24+19+58+199)
Error
Error_class_0 <- (19/(19+24))
Error_class_0 
Error_class_1 <- (58/(199+58))
Error_class_1




##5
par(mfrow=c(2,5)) 
set.seed(10)
Creditdata1[,-c(44)] <- scale(Creditdata1[,-c(44)])
N <- length(Creditdata1$Profitable)
train <- sort(sample(N, 0.7*N))
validate <- seq(N)[-train]
train_input <- Creditdata1[train,-44]
validate_input <- Creditdata1[validate,-44]
train_output <- Creditdata1[train,44]
validation_output <- Creditdata1[validate,44]
###
error_train_list = seq(15)
error_validate_list = seq(15)
for(i in seq(15)){
  prediction_train = knn(train_input, train_input, train_output, k=i)
  prediction_validate = knn(train_input, validate_input, train_output, k=i)
  error_train_list[i] = mean(abs(as.numeric(as.character(prediction_train))-train_output))
  error_validate_list[i] = mean(abs(as.numeric(as.character(prediction_validate))-validation_output))
} 
##
plot(c(1,15),c(0,0.5), type = "n", xlab ="k values", ylab="Error Rate")
lines(error_train_list, col="blue", type = 'l')  
lines(error_validate_list, col= "red", type = 'l')

min.error.validation <- which.min(error_validate_list)
min.error.validation
str(N)
head(N)

knn_predict <-prediction(attr(knn_predict,"prob"),validation_output)

##ROC CURVE FOR KNN
perf <- performance(knn_predict, measure = "tpr", "fpr" )
plot(perf, main = "ROC Curve for Profitable", col= "blue", lwd=3 )
abline(a=0, b=1, lwd=2, lty=2)

perf.auc<-performance(prediction_validate, measure="auc")
