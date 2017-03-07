# Logistic Regression
head(dataset)
# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[, 3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[, 1:2] = scale(training_set[, 1:2])
test_set[, 1:2] = scale(test_set[, 1:2])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-3])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred > 0.5)
cm
mean((test_set$Purchased != y_pred)^2)
# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

## Bagging

set.seed(20)


# Fitting Random Forest Classification to the Training set
# install.packages('randomForest')
library(randomForest)
set.seed(123)
rf.socialnetwork = randomForest(Purchased~.,  data = training_set, importance = TRUE)
yhat.rf = predict(rf.socialnetwork, newdata=test_set)
cm= table(test_set[,3], yhat.rf)
cm

importance(rf.socialnetwork)
errorte = (8+7)/ (56+8+7+29)
errorte

classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm
accuracy = (53+29)/(53+11+7+29)
accuracy
errorrate= (11+7)/(53+11+7+29)
errorrate
importance(classifier)
## bagging
set.seed(1)
bag.sc =randomForest(Purchased~., data = training_set, mtry = 2, importance = TRUE)
bag.sc
cm = table(test_set[, 3], y_pred)
cm

errorate2= (10+5)/(54+10+5+31)
errorate2
importance(bag.sc)
----
set.seed(1)
boost.sc = gbm(Purchased~., data = training_set, distribution = "bernoulli", n.trees = 300)
summary(boost.sc)
yhat.boost = predict(boost.sc, newdata = test_set, n.trees = 300, type = "response")
table(yhat.boost, test_set[,3])


##BOOSTING
library(gbm)
set.seed(1)
boost.fit <- gbm(Purchased~., data = training_set, distribution = "bernoulli", n.trees = 3000)
boost.probs <- predict(boost.fit, newdata = training_set, n.trees=3000)
boost.pred <- rep(0,length(boost.probs))
boost.pred[boost.probs > 0.5] =1

ifelse(boost.probs >0.5,1,0)
cm1 = table(test_set[,3], boost.pred)
library(caret)

alldata2 = rbind(training_set, test_set)
boost.fit <- gbm(Purchased ~., data = alldata2, distribution = "bernoulli", n.trees = 400)
boost.probs <- predict(boost.fit, newdata = test_set, n.trees = 300, type = "response")
boost.pred <- ifelse(boost.probs <0.5, 1, 0)
table(test_set$Purchased, boost.probs)
cm2 = table(boost.fit, boost.probs)
cm2

mean((boost.probs != test_set)^2)

socialnetwork.fit<-gbm(Purchased~., data=alldata2, dist="adaboost", n.tree =400, shrinkage = 1, train.fraction = (2/12))
par(ask=T)
gbm.perf(socialnetwork.fit)


(predict(socialnetwork.fit, test_set, n.trees = =69) >0, test_set$Purchased >0)
