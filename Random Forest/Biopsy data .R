library(MASS)
data("biopsy")
str(biopsy)
biopsy$ID = NULL

names(biopsy) = c("thick", "u.size", "u.shape", "adhsn", "s.size", "nucl", "chrom", "n.nuc", "mit", "class")
names(biopsy)

biopsy.v2 = na.omit(biopsy)
library(reshape2)
library(ggplot2)
biop.m = melt(biopsy.v2, id.var="class")
ggplot(data=biop.m, aes(x=class, y=value)) + geom_boxplot() +facet_wrap(~variable,ncol = 3)

set.seed(123)
ind = sample(2, nrow(biopsy.v2), replace=TRUE, prob=c(0.7, 0.3))
train = biopsy.v2[ind==1,] #the training data set
test = biopsy.v2[ind==2,] #the test data set
str(test) #confirm it worked

table(train$class)
table(test$class)

full.fit = glm(class~., family=binomial, data=train)
summary(full.fit)
confint(full.fit)
library(car)
vif(full.fit)
##check multicolinearirty
contrasts(train$class)

train$predict = rep("benign", 474)
train$predict[train$probs>0.5]="malignant"
table(train$predict, train$class)
mean(train$predict==train$class)

##TEST SET
test$prob = predict(full.fit, newdata=test, type="response")
test$predict = rep("benign", 209)
test$predict[test$prob>0.5]="malignant"
table(test$predict, test$class)
mean(test$predict==test$class)

##RANDOM FOREST CLASSIFICATION
rf.biop = randomForest(class~., data=biop.train)
