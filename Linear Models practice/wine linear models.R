wine <- read.csv('wine.csv')
str(wine)

head(wine,20)


tail(wine,10)

summary(wine)

## create one variable linear regression using avg.growing temperature to predict price
model1=lm(Price~AGST, data = wine)
summary(model1)

model1$residuals
## errors, reidusls
SSE = sum(model1$residuals^2)
# squares each residual term and adds them up
SSE

model2=lm(Price~ AGST + HarvestRain, data= wine)

model3=lm(Price~AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)

SSE1 = sum(model3$residuals^2)
SSE1
## which is better than the other model

model4= lm(Price~ AGST + HarvestRain + WinterRain +Age, data = wine)
summary(model4)

## is not stronger is adjusted increase now we see  now variable #3 age is significant
## mullticollineariy, france population and age is correlated.

cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
wineTest=read.csv("wine_test.csv")

str(wineTest)
predictTest= predict(model4, newdata=wineTest)
predictTest
## we want predict the model we used, and the new model

#3 we loook a the price 6.95, and 6.78,

## and compute r square test 
SSE = sum((wineTest$Price- predictTest)^2)
SST = sum((wineTest$Price- mean(wine$Price)^2))
1-SSE/SST
          