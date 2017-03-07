###Fitting Regression TREES

library(MASS)
library(tree)
attach(Boston)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test = -train

training_data = Boston[train,]
testing_data = Boston[test,]
testing_medv =medv[test]

head(Boston)
#FIT a tree based on training data 

tree_model = tree(medv~., training_data)
tree_model
plot(tree_model)
#LSAT #103 it contains 103 observations .30 # deviance
text(tree_model, pretty = 0)

### check how to model is doing using testing data set

tree_pred <- predict(tree_model, testing_data)
#mean_squared_error#
mean((tree_pred- testing_medv)^2)
##25.05 MSE

##PRUNING a  better MSE or may not able to reduce it
cv.tree = cv.tree(tree_model)

plot(cv.tree$size, 
     cv.tree$dev, 
     type = "b",
     xlab = "Tree Size",
     ylab = "MSE")
which.min(cv.tree$dev)
##gives you index what would be the size
cv.tree$size[1]
## prune the tree to size to 4 

pruned_model = prune.tree(tree_model, best = 4)

plot(pruned_model)
text(pruned_model, pretty = 0)
##check the accuracy of the model using testing data
tree_pred = predict(pruned_model, testing_data)
mean((tree_pred- testing_medv)^2)
