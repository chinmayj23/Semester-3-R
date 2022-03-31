library(ISLR)

Hitters
names(Hitters)
sum(is.na(Hitters))

Hitters = na.omit(Hitters)

dim(Hitters)

sum(is.na(Hitters))

## For subsetting and forward and backrd regression
## For regulisation we use library leaps
install.packages("leaps")
library(leaps)

subset.fit = regsubsets(Salary~.,data = Hitters)
summary(subset.fit)

## To get upto 19th row all the 19 models
subset.fit1 = regsubsets(Salary~.,data = Hitters,nvmax = 19)
summary(subset.fit1)

reg.summary = summary(subset.fit1)
reg.summary$rsq    ## R sqrd of all 19 models                        
reg.summary$adjr2

coef(subset.fit1,6)  ## To get the specific model

## Now carry out the regressions on 9 and 10 to check the mse for each  


## [plots] to check which model to the

plot(reg.summary$adjr2,type='p')

## Criteria for the best model

plot(subset.fit1,scale = "r2")  ##max

plot(subset.fit1,scale = "adjr2")  ##max

plot(subset.fit1,scale = "bic")  ##min

plot(subset.fit1,scale = "Cp")  ##min

## Above graphs will help get the specific models
coef(subset.fit1,6)
coef(subset.fit1,7)
coef(subset.fit1,8)



## *********************************** Sum on training set **********************************
set.seed(100)

split=sample.split(Hitters$Salary,SplitRatio = 0.8) 


training_set=subset(Hitters,split==T)


test_set=subset(Hitters,split==F)

sum(is.na(training_set))

subset.fit2 = regsubsets(Salary~.,data = training_set,nvmax = 19)
summary(subset.fit2)


reg.summary2 = summary(subset.fit2)
reg.summary2$rsq                            
reg.summary2$adjr2


plot(subset.fit2,scale = "r2")  ##max

plot(subset.fit2,scale = "adjr2") ##max

plot(subset.fit2,scale = "bic")  ##min

plot(subset.fit2,scale = "Cp")  ##min

## now found 3 models
coef(subset.fit2,6)
coef(subset.fit2,7)
coef(subset.fit2,14)


lm1<-lm(Salary~Hitters$Runs+Hitters$CAtBat+Hitters$CHits+Hitters$CRBI+Hitters$Division,data = Hitters)
summary(lm1)

y1=predict(lm1,newdata = test_set)
y1


sr1 = (y1 - test_set$Salary)^2
mse1 = mean(sr1)
mse1


lm2<-lm(Salary~Hitters$AtBat+Hitters$Hits+Hitters$Walks+Hitters$CRuns+Hitters$CWalks+Hitters$Division,data=Hitters)
summary(lm2)

y2<-predict(lm2,newdata = test_set)
y2

sr2 = (y2 - test_set$Salary)^2
mse2 = mean(sr2)
mse2



lm3<-lm(Salary~AtBat+Hits+HmRun+RBI+Walks+CAtBat+CHmRun+CRuns+CRBI+CWalks+Division+PutOuts+Assists+Errors,data = Hitters)
summary(lm3)

y3<-predict(lm3,newdata = test_set)
y3

sr3 = (y3 - test_set$Salary)^2
mse3 = mean(sr3)
mse3



mse
mse1
mse2
mse3
