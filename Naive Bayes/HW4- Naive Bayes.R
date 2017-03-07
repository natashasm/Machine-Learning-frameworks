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
install.packages("pROC")
library(pROC)
install.packages("e1071")
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
#since it is not the same as the training sample with which we are comparing for the nearest neighbors. 

min.error.validation <- which.min(error_validate_list)
min.error.validation

##explain
knn_predict <- knn(train_input, validate_input, train_output, k= 13)
knn_probability = knn(train_input, validate_input, train_output, k=13, prob = T)
knn_probability <- attr(knn_probability, "prob")
pred_knn <- prediction(knn_probability, validation_output)
perf_knn <- performance(pred_knn, "tpr", "fpr")

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




Creditdata1 <- read_excel("credit.xlsx",1)
Creditdata1$Y <- ifelse(Creditdata1$NPV> 0, 1, 0)

sum(Creditdata1$TYPE0)


Creditdata1$CHK_ACCT <- as.factor(Creditdata1$CHK_ACCT)
Creditdata1$SAV_ACCT <- as.factor(Creditdata1$SAV_ACCT)
Creditdata1$HISTORY <- as.factor(Creditdata1$HISTORY)
Creditdata1$PRESENT_RESIDENT <- as.factor(Creditdata1$PRESENT_RESIDENT)
Creditdata1$EMPLOYMENT <- as.factor(Creditdata1$EMPLOYMENT)
Creditdata1$JOB <- as.factor(Creditdata1$JOB)
Creditdata1$TYPE <- as.factor(Creditdata1$TYPE)
Creditdata1$RENT <- as.factor(Creditdata1$RENT)
Creditdata1$INSTALL_RATE <- as.factor(Creditdata1$INSTALL_RATE)
Creditdata1$GUARANTOR <- as.factor(Creditdata1$GUARANTOR)
Creditdata1$OTHER_INSTALL <- as.factor(Creditdata1$OTHER_INSTALL)
Creditdata1$OWN_RES <- as.factor(Creditdata1$OWN_RES)
Creditdata1$TELEPHONE<- as.factor(Creditdata1$TELEPHONE)
Creditdata1$FOREIGN <- as.factor(Creditdata1$FOREIGN)
Creditdata1$REAL_ESTATE<- as.factor(Creditdata1$REAL_ESTATE)
Creditdata1$TYPE<- as.factor(Creditdata1$TYPE)
Creditdata1$NUM_DEPENDENTS <- as.factor(Creditdata1$NUM_DEPENDENTS)
Creditdata1$NUM_CREDITS <- as.factor(Creditdata1$NUM_CREDITS)

attach(Creditdata1)
set.seed(1234)
N= length(Y)
train.id <- sample(N, 0.7*N)
dftrain <- Creditdata1[train.id,]
dfvalidation <- Creditdata1[-train.id,  ]
dfvalidation
dftrain$`OBS#`<- NULL
dfvalidation$`OBS#` <- NULL
dftrain$AGE <- NULL
dfvalidation$AGE <- NULL
dftrain$DURATION <- NULL
dfvalidation$DURATION <-NULL
dftrain$AMOUNT_REQUESTED <- NULL
dfvalidation$AMOUNT_REQUESTED <- NULL
dftrain$CREDIT_EXTENDED <- NULL
dfvalidation$CREDIT_EXTENDED <- NULL
dftrain$NPV <- NULL
dfvalidation$NPV <- NULL

dftrain$Y <- as.factor(dftrain$Y)
dfvalidation$Y <- as.factor(dfvalidation$Y)

##8
#model <- naiveBayes(Y~CHK_ACCT + SAV_ACCT + NUM_CREDITS  + HISTORY + PRESENT_RESIDENT + EMPLOYMENT + JOB +  NUM_DEPENDENTS + RENT + INSTALL_RATE + GUARANTOR + OTHER_INSTALL + OWN_RES + TELEPHONE + FOREIGN + REAL_ESTATE + TYPE, data=dftrain)
model <- naiveBayes(Y~., data = dftrain)

prediction <-predict(model, newdata = dfvalidation[,-18])
prediction_naive <-predict(model, newdata = dfvalidation[,-18], type = "raw")
pred_naive <- prediction(prediction_naive, dfvalidation[,18])
perf_naive <- performance(pred_naive, "tpr", "fpr")


## Exhibit 5
table(dfvalidation$Y, prediction, dnn = list('actual', 'predicted'))

## Exhibit 6
model

sum(Creditdata1$Y)


###ROC CURVE FOR NAIVE BAYES
set.seed(1234)
## Training Set
TRAIN <- sample(nrow(Creditdata1),.7*nrow(Creditdata1))
TRAIN_SET <- Creditdata1[TRAIN,]

## Validation Set
Validation_Credit <- Creditdata1[-TRAIN,]

creditfit2<- glm(Profit~ AGE + CHK_ACCT1 + CHK_ACCT2+ CHK_ACCT3 + SAV_ACCT1 + SAV_ACCT2 + 
                    SAV_ACCT3 + SAV_ACCT4+ NUM_CREDITS + DURATION  +HISTORY1 + HISTORY2 + 
                    HISTORY3 + HISTORY4+ PRESENT_RESIDENT2 + PRESENT_RESIDENT3 + 
                    PRESENT_RESIDENT4 + EMPLOYMENT1 + EMPLOYMENT2+ EMPLOYMENT3 + EMPLOYMENT4 + 
                    JOB1 + JOB2+ JOB3+ TYPE1+ TYPE2+ TYPE3+ TYPE4+ TYPE5 + TYPE6 + + NUM_DEPENDENTS + 
                    RENT + INSTALL_RATE + GUARANTOR + OTHER_INSTALL +  OWN_RES + TELEPHONE + FOREIGN + REAL_ESTATE + AMOUNT_REQUESTED, data = TRAIN_SET, family = 'binomial')
##
profit_probability <- predict(creditfit2, glm.type = "response", newdata = Validation_Credit)
pred <-(profit_probability, Validation_Credit$Profitable)
perf1 <- performance(pred, "tpr", "fpr")
plot(perf, col = "red")
pred_knn_roc <- prediction(attr(valid,"prob"),validation_output)
perf_knn_roc <- performance(pred_knn_roc, "tpr", "fpr")
plot(perf_knn_roc, col ="blue", add = TRUE)
pred_naive_probability<-predict(model, newdata = dfvalidation[,18], type = "raw")
pred_naive_roc <- prediction(pred_naive_probability[,-18],dfvalidation$Y)
perf_naive_roc <- performance(pred_naive_roc, measure = "tpr", "fpr")
plot(perf, main = "ROC Curve for Profitable", col= "blue", lwd=3 )
abline(a=0, b=1, lwd=2, lty=2)
auc(prediction$Y, prediction)


##ANSWER ###9
applicants_new <- data.frame(CHK_ACCT = "2", SAV_ACCT = "4", NUM_CREDITS = "1", HISTORY = "1", PRESENT_RESIDENT = "1", EMPLOYMENT = "1", JOB= "2", NUM_DEPENDENTS = "1", RENT = "1", INSTALL_RATE = "3", GUARANTOR = "0", OTHER_INSTALL = "0", OWN_RES = "0", TELEPHONE= "1", FOREIGN = "0", REAL_ESTATE = "0",TYPE ="2")
applicants_new             
prediction <-predict(model, newdata = applicants_new, type = "raw")
prediction<- predict(model, newdata = applicants_new)
prediction
CrossTable(prediction, applicants_new,
                     prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
                     dnn = c('predicted', 'actual'))


##ANSWER 10
Creditdata1 <- read_excel("credit.xlsx",1)
Creditdata1$Profitable <- ifelse(Creditdata1$NPV> 0, 1, 0)

Creditdata1 <- dummy.data.frame(Creditdata1, c("CHK_ACCT", "SAV_ACCT", "HISTORY", "PRESENT_RESIDENT","EMPLOYMENT", "JOB", "TYPE"))
##4Split the data into 2 parts, training (70%) and validation (30%). Set the seed to 12345.

set.seed(1234)
## Training Set
TRAIN <- sample(nrow(Creditdata1),.7*nrow(Creditdata1))
TRAIN_SET <- Creditdata1[TRAIN,]

## Validation Set
Validation_Credit <- Creditdata1[-TRAIN,]

creditfit2 <- glm(Profitable~ AGE + CHK_ACCT1 + CHK_ACCT2+ CHK_ACCT3 + SAV_ACCT1 + SAV_ACCT2 + 
                    SAV_ACCT3 + SAV_ACCT4+ NUM_CREDITS + DURATION  +HISTORY1 + HISTORY2 + 
                    HISTORY3 + HISTORY4+ PRESENT_RESIDENT2 + PRESENT_RESIDENT3 + 
                    PRESENT_RESIDENT4 + EMPLOYMENT1 + EMPLOYMENT2+ EMPLOYMENT3 + EMPLOYMENT4 + 
                    JOB1 + JOB2+ JOB3+ TYPE1+ TYPE2+ TYPE3+ TYPE4+ TYPE5 + TYPE6 + + NUM_DEPENDENTS + 
                    RENT + INSTALL_RATE + GUARANTOR + OTHER_INSTALL +  OWN_RES + TELEPHONE + FOREIGN + REAL_ESTATE + AMOUNT_REQUESTED, data = TRAIN_SET, family = 'binomial')
##
creditfit.predicted.validation = predict(creditfit2, type = "response", newdata = Validation_Credit)
actual.validation <- Validation_Credit$Profitable
##ROC for  validation
pred <- prediction(creditfit.predicted.validation, actual.validation)
perf <- performance(pred, "tpr", "fpr")
auc <- performance(pred, "auc")
auc.v <- unlist(slot(auc, "y.values"))
auc.v 
plot(perf, colorize = T)
plot(perf_naive, col = "blue", add = TRUE)
plot(perf_knn, col = "red", add = TRUE)


pred_knn_roc <- prediction(attr(valid,"prob"),validation_output)
perf_knn_roc <- performance(pred_knn_roc, "tpr", "fpr")
plot(perf_knn_roc, col ="blue", add = TRUE)
pred_naive_probability<-predict(model, newdata = dfvalidation[,18], type = "raw")
pred_naive_roc <- prediction(pred_naive_probability[,-18],dfvalidation$Y)
perf_naive_roc <- performance(pred_naive_roc, measure = "tpr", "fpr")
plot(perf, main = "ROC Curve for Profitable", col= "blue", lwd=3 )
abline(a=0, b=1, lwd=2, lty=2)
auc(prediction$Y, prediction)
