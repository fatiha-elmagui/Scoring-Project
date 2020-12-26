library(readr)
library(caret)
library(ggplot2)
library(sqldf)
require(ggplot2)
library(MASS)
require(MASS)
source("AFD_procedures.R")

ScoringTraining <- read_csv("/home/noamane/Downloads/scoring/ScoringTraining.csv")
ScoringTraining=ScoringTraining[,-1] 

1

p1 <- ggplot(ScoringTraining) + geom_bar(aes(x = factor(SeriousDlqin2yrs), y = ..count../sum(..count..))) + ylab("Proportion") + xlab("Classification")
Plot.Factor <- function(ScoringTraining){p1}
Plot.Factor(ScoringTraining )

2

boxplot(ScoringTraining$RevolvingUtilizationOfUnsecuredLines)
boxplot(ScoringTraining$SeriousDlqin2yrs)
boxplot(ScoringTraining$age)
boxplot(ScoringTraining$'NumberOfTime30-59DaysPastDueNotWorse')
boxplot(ScoringTraining$MonthlyIncome)
boxplot(ScoringTraining$NumberOfTimes90DaysLate)
boxplot(ScoringTraining$NumberOfDependents)
boxplot(ScoringTraining$'NumberOfTime60-89DaysPastDueNotWorse')

3

library(Amelia)
missmap(ScoringTraining, main = "Missing values vs observed")

4

data<-rbind(ScoringTraining) 
colnames(data)[apply(data,2,anyNA)] #les valeurs manquantes
data$age<-ifelse(data$age > 0,data$age,NA)
data$RevolvingUtilizationOfUnsecuredLines<ifelse(data$RevolvingUtilizationOfUnsecuredLines<=1,data$RevolvingUtilizationOfUnsecuredLines,NA)
data$`NumberOfTime30-59DaysPastDueNotWorse`<-ifelse(data$`NumberOfTime30-59DaysPastDueNotWorse` < 60,data$`NumberOfTime30-59DaysPastDueNotWorse`,NA)
data$NumberOfTimes90DaysLate<-ifelse(data$NumberOfTimes90DaysLate < 60,data$NumberOfTimes90DaysLate,NA)
data$`NumberOfTime60-89DaysPastDueNotWorse`<-ifelse(data$`NumberOfTime60-89DaysPastDueNotWorse` < 60,data$`NumberOfTime60-89DaysPastDueNotWorse`,NA)
data$DebtRatio<-ifelse(is.na(data$MonthlyIncome),NA,data$DebtRatio)
newdata <-na.omit(data)
missmap(newdata)
dim(newdata)

5

#le nombre de 0 et de 1 pour la variable SeriousDlqin2yrs
table(newdata$SeriousDlqin2yrs)
#convertir la variable en un facteur
fac <- factor(newdata$SeriousDlqin2yrs)
d<-downSample(newdata,fac)
table(d$SeriousDlqin2yrs)
d<-subset(d,select=-Class)

6

d$SeriousDlqin2yrs<-factor(d$SeriousDlqin2yrs, levels=c(1,0))
boxplot(d$age~d$SeriousDlqin2yrs,d) 
boxplot(d$RevolvingUtilizationOfUnsecuredLines~d$SeriousDlqin2yrs,d)
boxplot(d$`NumberOfTime30-59DaysPastDueNotWorse`~d$SeriousDlqin2yrs,d)
boxplot(d$DebtRatio~d$SeriousDlqin2yrs,d)
boxplot(d$MonthlyIncome~d$SeriousDlqin2yrs,d)
boxplot(d$NumberOfOpenCreditLinesAndLoans~d$SeriousDlqin2yrs,d)
boxplot(d$NumberOfTimes90DaysLate~d$SeriousDlqin2yrs,d)
boxplot(d$NumberRealEstateLoansOrLines~d$SeriousDlqin2yrs,d)
boxplot(d$`NumberOfTime60-89DaysPastDueNotWorse`~d$SeriousDlqin2yrs,d)
boxplot(d$NumberOfDependents~d$SeriousDlqin2yrs,d)

#PARTIE2
##QST7

X<- d[,2:11]
X<- as.data.frame(X)
y<- d[,1]
y<-as.vector(y)
y<- as.data.frame(y)

res<-AFD(X,y)
summary(res)
plotAFD(res)

##QST8

fitl<-lda(y~.,X)
plot(fit)

fitq<-qda(y~.,X)

##QST9

library(tidyverse)
library(glmnet)
library(caret)

d<-as.data.frame(d)
d<-as.numeric(d)

head(d)
X<-subset(d,select=-SeriousDlqin2yrs)
y<-d[,1]
afd<-AFD(X,y)
plotAFD(afd)

RegLogistique<-glm(d ~ SeriousDlqin2yrs,data=d,family=binomial("logit"))
stepAIC(trace=FALSE)
# Summarize the final selected model 
summary(RegLogistique)
#plot results of logistic regression
predicted=predict(RegLogistique,newdata=d,type="response")
plot(y,data=d,col="red4")
lines(y,preedicted,col="green4",lwd=2)