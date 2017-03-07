### Classification TREES
library(ISLR)
library(tree)## to fit decision tree  ## 11 variables, predict Sales basdd on sales all over variables
## continous variables classification convert to ifelse

attach(Carseats) 
head(Carseats)

##Start Data manipulation
range(Sales) ## Sales range from 0 to 16
High = ifelse(Sales >=8, "Yes", "No")
##split half 16

##appends High to carseat dataset and now our datset is ready
Carseats=data.frame(Carseats,High)

##Split data into testing and training using
set.seed(2)
train = sample(1:nrow(Carseats), nrow(Carseats)/2)
test = -train
training_data = Carseats[train,]


testing_data =  Carseats[test,]
testing_High = High[test]

##fitting our trees using the training data
tree_model= tree(High~., training_data)
## produce tree model
plot(tree_model)
text(tree_model, pretty = 0)