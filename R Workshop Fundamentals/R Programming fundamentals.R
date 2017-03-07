
red_balls<-5
blue_balls<-5

total<-red_balls+blue_balls
y <- c(2,4,5)
z <- c(5,6,8)
a <- y+z


height <- c(153, 160)
weight <- c (53, 50)
names(height) <- c("rini", "mansi")
names(weight) <- c("rini", "mansi")


BMI <- c(weight/(height*height))


vector <- c(1,2,3,4,5,6,7,8,9)


my_matrix <- matrix(vector,byrow = TRUE, nrow = 3)


rownames(my_matrix)<-c("a","b","c")
colnames(my_matrix)<- c("v","w","x")
my_matrix

rowSums(my_matrix)
colMeans(my_matrix)
sumofRows <- rowSums(my_matrix)
my_matrix_2 <- cbind(my_matrix, sumofRows)
my_matrix_2

sumofCol<- colSums(
  my_matrix_1 <-rbind(my_matrix,)
  
  ##first two columns
  ## you can type the numbers and column
  vector <- c(1,2,3,4,5,6,7,8,9)
  
  
  my_matrix <- matrix(vector,byrow = TRUE, nrow = 3)
  
  
rownames(my_matrix)<-c("a","b","c")
colnames(my_matrix)<- c("v","w","x")
my_matrix
  
sumOf_rows <- rowSums(my_matrix)
sumOfCols <- colSums(my_matrix)
  
my_matrix_1 <-rbind(my_matrix,sumOfCols)
my_matrix_1
  
my_matrix_2 <- cbind(my_matrix_1,sumOf_rows)
my_matrix_2

my_matrix_3


my_marix

color_vector <- c(140,90,20,30)
color_flower_vector<- c(-30,20,10,80,30)
color_vector_names<- c("Week1", "Week2", "Week3", "Week4", "Week5")
color_flower_names <- c("P", "R", "C", "D", "E")

color_matrix <- matrix(c(color_vector, color_flower_vector), nrow = 3, byrow=TRUE)
median()

##FACTORS
car_vector <- c("Honda", "Hundai", "Maruti", "Toyota")
class(car_vector)
car_vector

car_vector_factor <- factor(car_vector)
class(car_vector_factor)

car_vector_factor
### FACTORS
grade_vector <- c("A", "B", "C", "D")
class(grade_vector)

grade_vector

grade_vector_factor <- factor(grade_vector, ordered = TRUE, levels = c("D", "C", "B", "A"))
## nominal factor 
Smith_MS_majors_vector <- c("MSFIN", "MSSC", "MSIS")
class(Smith_MS_majors_vector)
Smith_MS_majors_vector

Smith_MS_majors_factor <- factor (Smith_MS_majors_vector)
class(Smith_MS_majors_factor)

## another vecotr ordinal
Car_speed_vector <- c("slow", "fast", "ultra-fast")

class(Car_speed_vector)
Car_speed_factor <- factor(Car_speed_vector, ordered = TRUE, levels= c("slow", "fast", "ultra-fast"))
class(Car_speed_factor)
##$, ## factor categorial column it hsould be numeric

name <- c("Tom", "Hank", "Jelly", "Sam", "Jessica", "Jada")
sex <- c("Male", "Male", "Female", "Male", "Female", "Female")
age <- c(26,22,24,29,21,26)
salary <- c(8500,7200,7900,10500,70000,85000)
house <- c(0,1,1,0,1,0)

test <- data.frame(Name=name, Sex=sex, Age=age, Salary=salary, House=house)
class(test)
test
## to see top and bottom rows 
head(test,2)
tail(test,3)
dim(test)
nrow(test)
ncol(test)
str(test)
## selecting columsn/variables  this is select a data frame
test["Sex"]
#select as a vector
test$Age

##changing data type of columns
test$Name <- as.character(test$Name);
str(test);
test[1,4];
test[1:2, 4:5];
test[3,];

##slicing data frame
test[c(1,3,5), c("Sex", "Age", "House")];
##convert house into a factor and it is a numeric column because it is  adummy v ariable
## you need convert name as a categorical column

test[3, "Salary"] <- 90000
test
##FILTERING DATA FRAMES

test$Salary > 80000
test[test$Salary > 80000, ]

age_list <- c(21,22,29)
test[test$Age %in% age_list, ]


##importing text
wine_quality <- read.csv("~/Downloads/R/winequality-white.txt", sep = ";")

#looking at the top few rows
head(wine_quality)


titantic <- read.csv("~/Downloads/R/Titanic-1.csv")
head(titantic,5)
tail(titantic,5)

str(titantic)
dim(titantic)
nrow(titantic)
ncol(titantic)
##change the data type 
titantic$PassengerId <- as.numeric(titantic$PassengerId)
class(titantic$PassengerId)
titantic$Survived <- as.factor(titantic$Survived)
class(titantic$Survived)
titantic$Pclass <- as.factor(titantic$Pclass)
class(titantic$Pclass)
titantic$Sex <- as.factor(titantic$Sex)
class(titantic$Sex)
titantic$Age <- as.numeric(titantic$Age)
class(titantic$Age)
titantic$SibSp <- as.numeric(titantic$SibSp)
class(titantic$SibSp)
titantic$Parch <- as.factor(titantic$Parch)
class(titantic$Parch)
titantic$Fare <- as.numeric(titantic$Fare)
class(titantic$Fare)
titantic$Cabin <- as.character(titantic$Cabin)
class(titantic$Cabin)
titantic$Embarked <- as.factor(titantic$Embarked)
class(titantic$Embarked)
##20 minute exercise 
Titantic_2 <- read.csv("~/Downloads/R/Titanic-2.csv")
Titantic_2
str(Titantic_2)
dim(Titantic_2)

test <- Titantic_2[titantic$Survived ==1,]
test <- Titantic_2[Titantic_2$Pclass %in% c(1,2),]
test



### conditional statement
num <- 5
if(num <10){
  print("Given Number is less than 10")
}

num<- 12
if(num <10){
  print("Given Number is less than 10")
}else{
  num=num+2
  print(num)
  print("Number is 2 more than original number")
}

num <- 17
if(num<10) {
  print("Given Number is less than 10")
}else if (num <15) {
    print("Given Number is less than 15")
} else{
    print("Given Number is greater than 15")
}


score<- 80
if(score>90){
  print ("A")
}else if (score > 80 & score(<=90){
  print("B")
} else if (score > 70 & score<=80 ){
  print("C")
}else if (score >60 & score <=70){
  print("D")
}else if (score <60)
  print("F")

if(score > 90){
  print("A")
} else if (score > 80 & score <= 90){
  print("B")
}else if (score>70 & score <=80){
  print("C")
} else {
  print("D")
}
##while loop

i <- 1

while (i > 6){
  print(i)
  i = i+1
}

## for loop
2010:2015
for(year in 2010:2015){
    print(paste("This year is", year))
}

y<- c("q", "w", "e", "r", "z", "c")
for(letters in y){
  print(letters)
}  
  i <- 1

  while( i <=7){
   if(i%%2 !=0){
     print(i)
  } 
    i=i+1
  }
  
  -------
    



sum(is.na(Titantic_2$Sex))
sum(is.na(Titantic_2$PassengerId))
sum(is.na(Titantic_2$Pclass))
sum(is.na(Titantic_2$Age))
sum(is.na(Titantic_2$SibSp))
sum(is.na(Titantic_2$Parch))
sum(is.na(Titantic_2$Fare))
sum(is.na(Titantic_2$Embarked))

Titantic_2<- Titantic_2[!is.na(Titantic_2$Age),]
Titantic_2

Titantic_2$Survived[is.na(Titantic_2$Survived)] <- 0

Titantic_2$Age[is.na(Titantic_2$Age)] <- median(Titantic_2$Age, na.rm = TRUE)

Titantic_2<- Titantic_2[is.na(Titantic_2$Embarked),]

sum_null <- function(x){
  sum(is.na(x))
}
apply(Titantic_2[,c(1,2,3,4,5,6,7,8,9)],2,sum_null)

summary(test$Salary)
summary(test$Age)
summary(test)
summary(test$Sex)
mean(test$Salary)
install.packages("dplyr")
library(dplyr)

Titantic_2$Pclass <- as.factor(Titantic_2$Pclass)
Titantic_2$Age <- as.factor(Titantic_2$Age)

Titantic_2$Survived[is.na(Titantic_2$Survived)] <- 0

Titantic_2$Age[is.na(Titantic_2$Age)] <- median(Titantic_2$Age, na.rm = TRUE)

Titantic_2<- Titantic_2[is.na(Titantic_2$Embarked),]
Titantic_2 <- group_by(Titantic_2, Pclass)

summarise(Titantic_2, avg_age=mean(Age))
2
plot(test$Sex)
count<- table(test$Sex)

barplot(count, main= "Gender Distribution", xlab= "Categories", ylab ="counts", col = "pink")

count <- table(test$Sex, test$House)
barplot(count, main ="Gender vs. House", xlab = "house", ylab ="count", col = c("Blue", "Red"),
        beside = TRUE, legend.text = row.names(count), args.legend = list(x=6.5, y=2.5, bty ="n"))

test

hist(test$Age, main = "Age Distribution", xlab = "Age", col = "yellow")

boxplot(titantic$Fare, main = "boxplot", col = "green")
boxplot(titantic$k~titantic$Fare, col = "pink")

bixokit