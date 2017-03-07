library(randomForest)
library(MASS)
set.seed(1)
attach(Boston)
train = sample(seq(dim(Boston)[1]), dim(Boston [1]/2))

bag.boston=randomForest(medv~., data=Boston, subset = train, mtry=13, importance=TRUE)               
bag.boston

boston.test = Boston[-train, "medv"]
yhat.bag = predict(bag.boston, newdata = Boston[-train])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

set.seed(1)
rf.boston = randomForest(medv~., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf = predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf-boston.test)^2)

library(gbm)

boost.boston =gbm(medv~., data = Boston[-train,], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

yhat.boost = predict(boost.boston, newdata = Boston[-train], n.trees=5000)
yhat.boost=predict(boost.boston,newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost-boston.test)^2)

###----
library(xgboost)
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test
bst <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nround = 2, objective = "binary:logistic")
## [0] train-error:0.046522
## [1] train-error:0.022263

pred <- predict(bst, test$data)
mean((test$label-pred)^2)
# [1] 0.02752428
table(test$label)

maxs <- apply(d)


