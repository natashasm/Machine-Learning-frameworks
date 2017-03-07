# Name = Natasha Medina
# Course: BUDT758T-0501
# HW6
##################################
##################################
# ANS 1
library(readxl)
setwd("/Desktop/BUDT758T - Data Mining/HW6")
getwd()
Credit = read_excel("credit.xlsx") #total = 1000 rows
set.seed(12345)

Credit$AMOUNT_REQUESTED  <- as.numeric(sub(',','',as.character(Credit$AMOUNT_REQUESTED),fixed=TRUE))
Credit$CREDIT_EXTENDED  <- as.numeric(sub(',','',as.character(Credit$CREDIT_EXTENDED),fixed=TRUE))
Credit$NPV  <- as.numeric(sub(',','',as.character(Credit$NPV),fixed=TRUE))
Profitable=rep(0,nrow(Credit))
Profitable[Credit$NPV > 0.0]=1
Credit$Profitable = Profitable

library(dummies)
CHK_ACCT = dummy(Credit$CHK_ACCT)
SAV_ACCT = dummy(Credit$SAV_ACCT)
HISTORY = dummy(Credit$HISTORY)
JOB = dummy(Credit$JOB)
TYPE = dummy(Credit$TYPE)
Credit = data.frame(Credit,CHK_ACCT,SAV_ACCT,HISTORY,JOB,TYPE)

Credit_full <- Credit

Credit$OBS. <- NULL
Credit$CHK_ACCT <- NULL
Credit$SAV_ACCT <- NULL
Credit$HISTORY <- NULL
Credit$JOB <- NULL
Credit$TYPE <- NULL
Credit$NPV <- NULL
Credit$CREDIT_EXTENDED <- NULL

##################################
##################################
# ANS 2

Credit_scale <- scale(Credit[,-16])
km.out <- kmeans(Credit_scale, 5, nstart = 20)
km.out
km.out$centers #Exhibit 1

km.out$withinss
km.out$betweenss
km.out$totss
km.out$tot.withinss
##################################
##################################
# ANS 3

library("DMwR", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
unscale(km.out$centers,Credit_scale)
install.packages("DMwR")
library(DMwR)
##################################
##################################
# ANS 4
NPV_cluster_table <- data.frame(Credit_full$NPV,km.out$cluster)
Count_people_cluster <- table(Credit_full$Profitable,km.out$cluster)
rownames(Count_people_cluster) <- c("Loss","Profit")
Perc_table = data.frame(cluster_no = numeric(5), Perc = numeric(5))
for(i_perc in 1:5)
  {
    Perc_table$Perc[i_perc] = sum(Count_people_cluster[,i_perc]) * 100/sum(Count_people_cluster)
    Perc_table$cluster_no[i_perc] = i_perc
  }

barplot(Perc_table$Perc,ylim = c(0,40),xlab = "Cluster Number",ylab ="Percentage",main = "Percentage of people in each cluster",col = "orange",names.arg = c("1","2","3","4","5"))

Avg_NPV_table <- data.frame(cluster_no = numeric(5), Avg_NPV = numeric(5))
for(i_avgNPV in 1:5)
{
  Sum_NPV = sum(ifelse(NPV_cluster_table$km.out.cluster == i_avgNPV, NPV_cluster_table$Credit_full.NPV,0))
  Avg_NPV_table$Avg_NPV[i_avgNPV] = Sum_NPV/sum(Count_people_cluster[,i_avgNPV])
  Avg_NPV_table$cluster_no[i_avgNPV] = i_avgNPV
}

Avg_NPV_table 
#Exhibit 2
#    Cluster_no    Avg_NPV
#1          1     -333.45270
#2          2     -255.27389
#3          3      85.90857
#4          4     -65.03559
#5          5     -13.43515
##################################
##################################
# ANS 6

##################
#K=4
##################
km.out4 <- kmeans(Credit_scale, 4, nstart = 20)
km.out4

km.out4$withinss
km.out4$betweenss
km.out4$totss
km.out4$tot.withinss

NPV_cluster_table4 <- data.frame(Credit_full$NPV,km.out4$cluster)
Count_people_cluster4 <- table(Credit_full$Profitable,km.out4$cluster)
rownames(Count_people_cluster4) <- c("Loss","Profit")
Perc_table4 = data.frame(cluster_no = numeric(4), Perc = numeric(4))
for(i_perc in 1:4)
{
  Perc_table4$Perc[i_perc] = sum(Count_people_cluster4[,i_perc]) * 100/sum(Count_people_cluster4)
  Perc_table4$cluster_no[i_perc] = i_perc
}

barplot(Perc_table4$Perc,ylim = c(0,40),xlab = "Cluster Number",ylab ="Percentage",main = "Percentage of people in each cluster",col = "orange",names.arg = c("1","2","3","4"))

Avg_NPV_table4 <- data.frame(cluster_no = numeric(4), Avg_NPV = numeric(4))
for(i_avgNPV in 1:4)
{
  Sum_NPV = sum(ifelse(NPV_cluster_table4$km.out4.cluster == i_avgNPV, NPV_cluster_table4$Credit_full.NPV,0))
  Avg_NPV_table4$Avg_NPV[i_avgNPV] = Sum_NPV/sum(Count_people_cluster4[,i_avgNPV])
  Avg_NPV_table4$cluster_no[i_avgNPV] = i_avgNPV
}

Avg_NPV_table4 

##################
#K=6
##################
km.out6 <- kmeans(Credit_scale, 6, nstart = 20)
km.out6
km.out6$withinss
km.out6$betweenss
km.out6$totss
km.out6$tot.withinss

NPV_cluster_table6 <- data.frame(Credit_full$NPV,km.out6$cluster)
Count_people_cluster6 <- table(Credit_full$Profitable,km.out6$cluster)
rownames(Count_people_cluster6) <- c("Loss","Profit")
Perc_table6 = data.frame(cluster_no = numeric(6), Perc = numeric(6))
for(i_perc in 1:6)
{
  Perc_table6$Perc[i_perc] = sum(Count_people_cluster6[,i_perc]) * 100/sum(Count_people_cluster6)
  Perc_table6$cluster_no[i_perc] = i_perc
}

barplot(Perc_table6$Perc,ylim = c(0,40),xlab = "Cluster Number",ylab ="Percentage",main = "Percentage of people in each cluster",col = "orange",names.arg = c("1","2","3","4","5","6"))

Avg_NPV_table6 <- data.frame(cluster_no = numeric(6), Avg_NPV = numeric(6))
for(i_avgNPV in 1:6)
{
  Sum_NPV = sum(ifelse(NPV_cluster_table6$km.out6.cluster == i_avgNPV, NPV_cluster_table6$Credit_full.NPV,0))
  Avg_NPV_table6$Avg_NPV[i_avgNPV] = Sum_NPV/sum(Count_people_cluster6[,i_avgNPV])
  Avg_NPV_table6$cluster_no[i_avgNPV] = i_avgNPV
}

Avg_NPV_table6

##################################
##################################
#Association Rules
##################################
##################################
# ANS 7
install.packages(c("arules", "arulesViz"))
library("arulesViz", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")
library("arules", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

setwd("/Users/priyashshahane/Desktop/umcp spring 2016/BUDT758T - Data Mining/HW6")
getwd()
Credit_df = read_excel("credit.xlsx") #total = 1000 rows
set.seed(12345)

Credit_df$NPV  <- as.numeric(sub(',','',as.character(Credit_df$NPV),fixed=TRUE))
Profitable=rep(0,nrow(Credit_df))
Profitable[Credit_df$NPV > 0.0]=1
Credit_df$Profitable = Profitable

library(dummies)
CHK_ACCT = dummy(Credit_df$CHK_ACCT)
SAV_ACCT = dummy(Credit_df$SAV_ACCT)
HISTORY = dummy(Credit_df$HISTORY)
EMPLOYMENT = dummy(Credit_df$EMPLOYMENT)
JOB = dummy(Credit_df$JOB)
OWN_RES = Credit_df$OWN_RES

dfCredit = data.frame(CHK_ACCT, SAV_ACCT, HISTORY, EMPLOYMENT, OWN_RES, JOB, Profitable)

dfCredit_matrix = as.matrix(dfCredit)

rules1 <- apriori(dfCredit_matrix, parameter = list(minlen=2, supp = 0.01, conf = 0.7), appearance= list(default="lhs",rhs="Profitable"),control= list(verbose=F))
rules1
rules2 <- apriori(dfCredit_matrix, parameter = list(minlen=2, supp = 0.01, conf = 0.6), appearance= list(default="lhs",rhs="Profitable"),control= list(verbose=F))
rules2
rules3 <- apriori(dfCredit_matrix, parameter = list(minlen=2, supp = 0.01, conf = 0.5), appearance= list(default="lhs",rhs="Profitable"),control= list(verbose=F))
rules3
rules4 <- apriori(dfCredit_matrix, parameter = list(minlen=2, supp = 0.01, conf = 0.4), appearance= list(default="lhs",rhs="Profitable"),control= list(verbose=F))
rules4
rules5 <- apriori(dfCredit_matrix, parameter = list(minlen=2, supp = 0.01, conf = 0.3), appearance= list(default="lhs",rhs="Profitable"),control= list(verbose=F))
rules5
rules6 <- apriori(dfCredit_matrix, parameter = list(minlen=2, supp = 0.1, conf = 0.8), appearance= list(default="lhs",rhs="Profitable"),control= list(verbose=F))
rules6
rules7 <- apriori(dfCredit_matrix, parameter = list(minlen=2, supp = 0.2, conf = 0.8), appearance= list(default="lhs",rhs="Profitable"),control= list(verbose=F))
rules7
rules <- apriori(dfCredit_matrix, parameter = list(minlen=2, supp = 0.01, conf = 0.8), appearance= list(default="lhs",rhs="Profitable"),control= list(verbose=F))
rules
inspect(rules)
?apriori
?parameter
?minlen
##################################
##################################
# ANS 8

sort.rules <- sort(rules, decreasing=TRUE, by=c("confidence","support"))
inspect(sort.rules[1:5])

##################################
##################################
# ANS 9

sumProfit = sum(Credit_df$NPV[Credit_df$JOB == 2 & Credit_df$CHK_ACCT == 3 & Credit_df$HISTORY == 4 & Credit_df$EMPLOYMENT == 4])
sumProfit

##################################
##################################
#Combining Cluster Analysis and Association Rules
##################################
##################################
# ANS 10

cluster_dummy = dummy(km.out$cluster)
df_cluster <- data.frame(Credit_full$Profitable, cluster_dummy)

df_cluster_matrix = as.matrix(df_cluster)

#Association rules
rules1 <- apriori(df_cluster_matrix, parameter=list(minlen = 2, supp=0.01,conf = 0.08),
                  appearance = list(default="lhs",rhs="Credit_full.Profitable"),
                  control = list(verbose=F))
rules1

#Top five rules
sort.rules1<-sort(rules1, decreasing=TRUE, by=c("confidence","support"))
inspect(sort.rules1[1:5])

NPV_cluster_table1 <- data.frame(Credit_full$NPV,km.out$cluster)

Sum_NPV1 = sum(ifelse(NPV_cluster_table1$km.out.cluster == 5, NPV_cluster_table1$Credit_full.NPV,0))
Sum_NPV1
#-3211

##################################
##################################