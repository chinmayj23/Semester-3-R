library(MASS)
library(ISLR)
library(foreign)

Boston

lm.fit<-lm(medv~.,data=Boston)

lm.fit1<-lm(medv~lstat,data=Boston)
summary(lm.fit1)
plot(lm.fit1)

## Solving non - linearity
lm.fit2<-lm(medv~lstat+I(lstat^2)+I(lstat^3)+I(lstat^4),data=Boston)
summary(lm.fit2)

anova(lm.fit1,lm.fit2)
## HEre the p value of anova is < 0.5 hence we accept the alternate hypothesis 
## where we accept the lm.fit1  model which is non - linear


## Correlation of error

lm.fit$residuals

## Non - constant variance of error terms

plot(lm.fit1)


## High leverage points
which.max(hatvalues(lm.fit1))
 

## Collinearity

lm.fit3<-lm(medv~lstat+age,data=Boston)
summary(lm.fit3)

lm.fit4<-lm(age~lstat,data=Boston) ## keeping age dependent 
summary(lm.fit4)

vif = (1/(1-0.3628))
vif ## variance inflation factor

install.packages('car')

lm.fit<-lm(medv~,data=Boston)
summary(lm.fit)
vif(lm.fit)


## 

lm1<-lm(Boston$medv~poly(Boston$lstat,4))
summary(lm1)

## ************************ Sir's Code *****************************


install.packages("caTools")


library(MASS)
library(ISLR)
library(car)
Boston
##MultiCollinearity
lm.fit<-lm(medv~.,data=Boston)
summary(lm.fit)
vif(lm.fit)  ##variance inflation factor
##Non-Linear
lm.fit1<-lm(medv~lstat,data=Boston)
lm.fit2<-lm(medv~lstat+I(lstat^2),data=Boston)
##better approach
lm.fit3<-lm(medv~poly(lstat,2),data=Boston)
anova(lm.fit1,lm.fit2)

##High Leverage Point
lm.fit4<-lm(medv~lstat,data=Boston)
plot(lm.fit4)
plot(hatvalues(lm.fit4))
which.max (hatvalues (lm.fit4))

##prediction
library(caTools)  ## Splits the given data sets randomly into test and training 

set.seed(123)     ## To get same results we keep set.seed(123) otherwise we can have any number
                  ## So to keep it same we use 123

split=sample.split(Boston$medv,SplitRatio = 0.8) ## command -> sample.split(dependent variable , split ration = x% or split ratio)
## preferably take the ratio as 0.8 and 0.2


training_set=subset(Boston,split==T)
## Now select train set and run it 


test_set=subset(Boston,split==F)

names(training_set)
regressor = lm(medv~.-age-indus,data=training_set)
## first run entire and check the summary and then
## remove age and then check the summary
## now finally remove indus as well
## You will get the best model finally


summary(regressor)


## Now we have to predict on the rmaining 0.2 ratio of data sets using predict function
## on the previous lm.fit which is actually the regressor over here
y_pred=predict(regressor,newdata = test_set)
y_pred
seq(1:72)
library(ggplot2)
ggplot() +
  geom_line(aes(x = seq(1:72), y = test_set$medv),
            colour='red') +
  geom_line(aes(x = seq(1:72), y = predict(regressor,newdata = test_set)),
            colour = 'blue')+
  ylab("Actual vs Prediction")+
  xlab("obs")
















