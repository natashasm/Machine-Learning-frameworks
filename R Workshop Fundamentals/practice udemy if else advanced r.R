# & AND
#! OR
# ! NOT

## LOGICAL OPERATORS

x<-10

x <20 & x >5
(x <20) & (x>5) & (x==10)

(x <20) & (x>5) & (x==9)

(x==10) | (x==100)
## one of these has to be true

(x=10000) | (x==100)

## is this not TRUE

!(10==1)
## is this not true that 10 equal to 1
!!(10==1)

df <- mtcars

head(mtcars)
df[df$mpg >20,]

df[df$mpg >20,'mpg']

subset(df,mpg>20)

df[(df$mpg>20) & (df$hp >100),]
## we ask and

df[(df$mpg>20) | (df$hp >100),]
## we ask for or 

#IF ELSE STATEMENTS
x<-13
if(x==10){
  #code
  print('X is equal to 10!')
}else if( x ==12){
  print(' x equal to 12')
} else{
  print(' x was not equal to 10 or 12')
}
  
## conditions in sie the paranethesis, and else 
temp <- 10
if(temp>80){
  #execute if condition was TRUE
  print('Temp is greater than 80')
}

if(temp >80){
  hot<-TRUE
}
print(hot)

temp <- 10

if (temp > 80){
  hot <- TRUE
  
}

hot

##IF ELSE BRACKET INDENTS with python is key but in  R
temp <- 30
if(temp>80){
  #execute if condition was TRUE
  print('Temp is greater than 80')
}else{
  print("Temp is not greater than 80")
}

#3multiple conditions checks to execute none match up
temp <- 30
if(temp>80){
  #execute if condition was TRUE
  print('Temp is greater than 80')
}elseif (temp<=80 & temp >=50){
  print("Nice outside")
}

## else if

temp <- 30

if (temp >80){
  print("Hot outside!")
} else if(temp<80 & temp >50){
  print("Nice outside!")
} else if (temp<50 & temp >32){
  print("It's cooler outside!")
} else{
  print("It's really cold outside!")
}


## another if else statement

temp <- 75

if(temp >80){
  print("Hot outside!")
}else if (temp<80 & temp >50){
  print("Nice outside!")
} else if(temp<50 & temp >32){
  print("its coller outside!")
}else{
  print("Its really cold outside")
}
####

ham <- 10
cheese <- 10
report <- 'blank'

if(ham >= 10 & cheese >=10){
  report <-"Strong sales of both ham and cheese"
}else if (ham == 0 & cheese ==0){
  report <- "No sales today!"
}else {
  report <- "We sold something today!"
}
print(report)

## write a script that prints "Hello" if the variable x is equal to 1

x<- 1

if(x==1){
  print("Hello")
}

x<-3
if(x%%2 ==0){
  print("Even Number")
}else{
  print('Not even')
}

x <- matrix()

if(is.matrix(x)){
  print('Is a matrix')
}else{
  print("Not a Matrix")
}
?is.matrix
Exercise 3
as.matrix(x)

is.matrix(x)

x<- c(3,7,1)

if( x[1] > x[2]){
  fir <- x[1]
  sec <- x[2]
}else{
  fir <- x[2]
  sec <- x[1]
}
if(x[3]>fir & x[3] >sec){
  thi <- sec
  sec <- fir
  fir <- x[3]
}else if (x[3] < fir & x[3] < sec){
  thi <- x[3]
} else{
  thi <- sec
  sec <- x[3]
}
print(paste(fir,sec, thi))

## write a script a that uses if , else if, and else statement to print the code
x<- c(20,10,1)



if (x[1] > x[2] & x[1] > x[3] ) {
  print(x[1] )
} else if (x[2] > x[3] ) {
  print(x[2])
} else {
  print(x[3])
}


is.logical(TRUE)
is.logical(12)


x <- matrix(nrow = 2)

if(is.matrix(x)){
  print("IS Matrix")
}else{
  print("Not a Matrix")
}


mat3 <- matrix(1:50, byrow = TRUE, nrow = 5)
if(is.matrix(mat3)){
  print("Is Matrix")
}else {
  print("Not a Matrix")
}


x<- 0

while(x<10){
  print(paste0("X is",x))
  
  x <-x+1
  if (x==10){
  print("X is now equal to 10! Break Leg")
  break
  print("Woo I printed too!")
  }
}

v<- c(1,2,3,4,5)

for(temp.var in v) {
  ## execute some code
  #for every temp.var in v
  print(temp.var)
}

for(temp.var in v){
  result <- temp.var +1
  print('The temp.var plus is equal to :')
  print(result)
}

my.list <- list(c(1,2,3), mtcars,12)
my.list

for(item in my.list){
  print(item)
}

## first vector

mat <- matrix(1:25,nrow = 5)
print(mat)
1:nrow(mat)
## vector 5 rows of matrix

for(row in 1:nrow(mat)){
  for(col in 1:ncol(mat)){
    print(paste('The elements at row:',row,'and col:', col,'is,mat',[row,col]))
  
    }
}

for (row in 1:nrow(mat)){
  for (col in 1:ncol(mat)){
    print(paste('The element at row:',row,'and col:',col,'is',mat[row,col]))
  }
}

## Functions

##  no inputs create a simple 

hello <- function(){
  print("Hello")
}
hello()

## no inputs in this  just prints hello
#hello
#no hello don't call function'
## temporary variable name
hello <- function(name='Frank'){
  #code exectes when the functoin is called
  print(paste('Hello', name))
}
## it is a default incase your users don't use it
hello('Sammy')
## no defaults below num1 1num2 
add_num <- function(num1, num2){
  my.sum <- num1+ num2
  return(my.sum)
}
 result <- add_num(4,5)