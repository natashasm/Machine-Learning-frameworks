library(MASS)
install.packages("tree")
library(tree)
attach(Boston)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
test= -train
training_data = Boston[train,]
testing_data = Boston[test,]
testing_medv = medv[test]

head(Boston)
#fit a tree based on training_data

tree_model <- tree(medv~., training_data)
tree_model
plot(tree_model)
text(tree_model, pretty = 0 )
#lsat contains 103 variables, variance: 30.13 point value 

##check how the model is doing using the testing dataset

tree_pred = predict(tree_model,testing_data)
table(tree_pred,testing_medv)
mean((tree_pred-testing_medv)^2)
## testing against the mean average values against predictions only for classifcation
## for regression you check MSE difference beween squares 25.04 is the results

## pruning probably to try achieve a better MSE or not.
## in order to be size of tree, we need to get number of leaves nodes, we do cross validation
cv_tree= cv.tree(tree_model)
names(cv_tree)
##difference sizes of corresponding to error rate
plot(cv_tree$size, 
     cv_tree$dev,
     type = "b", 
     xlab = "Tree Size",
     ylab = "MSE") 
which.min(cv_tree$dev)
## give you index of MMSE, if you want to see what would be the size 
cv_tree$size[1]

## prune the tree to size 4

pruned_model = prune.tree(tree_model, best = 4)
plot(pruned_model)
text(pruned_model, pretty = 0 )

## check the accuracy of the model using testing data
tree_pred = predict(pruned_model, testing_data)
mean((tree_pred-testing_medv)^2)


