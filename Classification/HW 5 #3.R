data(iris)
head(iris)

str(iris)
summary(iris)


##visualizing relationship between variables

library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species, shape = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, col = Species, shape = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Length, y = Petal.Width, col = Species, shape = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Width, y = Petal.Length, col = Species, shape = Species)) +
  geom_point()

ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, col = Species, shape = Species)) +
  geom_point()

ggplot(iris, aes(x = Petal.Width, y = Petal.Length, col = Species, shape = Species)) +
  geom_point()

##  split the data set to train and test

n <- length(iris[,1])
index1 <- 1 : n
# divide to 5 part of data
index2 <- rep(1 : 5, ceiling(n / 5))[1 : n]
set.seed(100)
# melt the order of the data
index2 <- sample(index2, n)
# get the one part of the data
m <- index1[index2 == 1]
trainset <- iris[-m, ]
testset <- iris[m, ]
str(trainset)


## linear discriminant analysis
library(MASS)
lda_model <- lda(Species ~ ., data = trainset)
lda_model
plot(lda_model)
lda_pred <- predict(lda_model, testset)

table(lda_pred$class, testset$Species)
mean(lda_pred$class == testset$Species)

## bagging
library(randomForest)
bag_model <- randomForest(ISv ~ ., data = trainset, mtry = 4,
                          importance = TRUE)
bag_model
bag_pred <- predict(bag_model, newdata = testset)
plot(bag_pred, testset$Species)
importance(bag_model)
table(bag_pred, testset$Species)
mean(bag_pred == testset$Species)

##  random forestlibrary(randomForest)
rf_model <- randomForest(Species ~ ., data = trainset, 
                         importance = TRUE)
rf_model
rf_pred <- predict(rf_model, newdata = testset)
plot(rf_pred, testset$Species)
importance(rf_model)
varImpPlot(rf_model)

table(rf_pred, testset$Species)
mean(rf_pred == testset$Species)

## boosting
library(gbm)
boost_model <- gbm(Species ~ ., data = trainset)
summary(boost_model)
boost_pred <- predict(boost_model, newdata = testset, n.trees = 100,
                      type = "response")

temp = data.frame(boost_pred[, , 1])
temp2 = apply(temp, 1, which.max)
boost_pred2 <- names(temp)[temp2]

table(boost_pred2, testset$Species)
mean(boost_pred2 == testset$Species)

###logistic regresion

iris2 <- iris  ## added whether or not is virginica speciies 
iris2$virginica <- iris$Species == "virginica"
fit4 <- glm(virginica ~ Petal.Width + Petal.Length + Sepal.Length + Sepal.Width, data=iris2, family=binomial)
plot(fit4)

predicted_preferences <- ifelse(iris2$virginica > 0.5,1,0)
iris2 
str(iris2)
iris
str(iris)

## boosting

##bagging
library(rpart)
library(adabag)
library(ipred)
iris.bagging = bagging(virginica~., data = iris2)
