a<-read.csv("C:/Users/voraa/Desktop/DATA SCIENCE/Advertising.csv")
#lm->linear model
#method 1:
lm.fit<-lm(sales~TV+radio+newspaper,data=a)
#metod 2:
attach(a)
lm.fit2<-lm(sales~TV+radio+newspaper)
#method 3:
lm.fit1<-lm(a$sales~a$TV+a$radio+a$radio)
names(lm.fit)
#coefficients are alpha,beta1,beta2,beta3
#residuals are your errors,i.e., y-yhat OR original-predicted
#fitted value-> presdicted values OR yhat
lm.fit$coefficients
lm.fit$residuals
lm.fit$fitted.values
summary(lm.fit)
#if in y=alpha+betA*x is same as the caluclated mean, h0:alpha=0
tcal=(2.93-0/0.312)
tcal
#match beta values similarly
lm.fitall<-lm(sales~.-X,data=a)
summary(lm.fitall)
#if company is increasing the sales by 1 unit while others remain constant, the sales will increase by 0.045 units
#if company doesnt change everything, on an everage sales will be 2.93 units
#removing newspaper since company doesnt make any gains through it
lm.nonp<-lm(sales~.-X-newspaper,data=a)
summary(lm.nonp)
#r-squared = estimated sum of squares / total sum of squares OR
#r-squared=1-(RSS/TSS) where TSS=RSS+ESS -> ess is estimated sum of squares,tss is total sum of squares, rss is residual sum of squares
#TSS is basically variability of the complte system
#targetted r-squared should be more than 0.9
#adjusted r-squared only changes if the variable that has impact on it sales is considered
#if r-squared changes with addition of variables, it is obsolete
lm.fitX<-lm(sales~.,data=a)
summary(lm.fitX)
#adjusted r-squared=1-((RSS/(n-k))/(TSS/(n-k))) ->k is no. of parameters, n is no. of obs
lm.fita<-lm(sales~TV+radio+(TV*radio),data=a)
summary(lm.fita)
lm.fitb<-lm(sales~TV+radio+newspaper+(TV*radio)+(TV*newspaper)+(radio*newspaper)+(TV*newspaper*radio),data=a)
summary(lm.fitb)
lm.fitc<-lm(sales~radio+newspaper+(radio*newspaper),data=a)
summary(lm.fitc)
library(ISLR)
?Auto
names(Auto)
A.fit1<-lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
summary(A.fit1)
A.fit2<-lm(mpg~displacement+year+weight+origin,data=Auto)
summary(A.fit2)
A.fit3<-lm(mpg~year+weight+origin,data=Auto)
summary(A.fit3)
A.fit4<-lm(mpg~year+weight+origin+(year*weight)+(year*origin)+(weight*origin)+(year*weight*origin),data=Auto)
summary(A.fit4)

