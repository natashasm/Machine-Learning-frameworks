
train.data <- read.csv("titanic-train.csv")
test.data <- read.csv("titanic-test.csv")
str(train.data)

library(caret)
set.seed(1)
inTrain<- createDataPartition(train.data$Survived,p=0.8, list=FALSE)
training<-train.data[inTrain,]
validation<-train.data[-inTrain,]
library(nnet)

training$Survived =class.ind(training$Survived)
validation$Survived = class.ind(validation$Survived)

#fit a neural network for classification purposes:
fitnn <- nnet(Survived~Sex+Age+Pclass, training, size=1, softmax=TRUE)
fitnn
summary(fitnn)

#Evaluate the overall performance of the neural network by looking at a tableof how predictions using the testing data.
table(data.frame(predicted=predict(fitnn, validation)[,2] > 0.5, actual=validation$Surv[,2]>0.5))
#In this evaluation, the probability more than 0.5 will be labelled as “Survived”. 

predicted=predict(fitnn, test.data) [,2]
predicted[is.na(predicted)]<-0
predicted[predicted >0.5]<-1
predicted[predicted <0.5]<-0
test.data$Survived<-predicted
test.data$Survived
str(test.data)
write.csv(test.data[,1:12], "nnet-result1.csv", row.names = FALSE)
