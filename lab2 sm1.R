Boston
library(MASS)
library(ISLR)
lm.poly<-lm(medv~poly(lstat,5),data=Boston)
summary(lm.poly)
#ca tools is a lbrary to split the data sets
library(caTools)
set.seed(123)
split<-sample.split(Boston$medv,SplitRatio=0.8)
trainng_set=subset(Boston,split==T)
test_set=subset(Boston,split==F)
names(trainng_set)
regressor<-lm(medv~.-age-indus,data=trainng_set)
summary(regressor)
y_pred=predict(regressor,newdata = test_set)
y_pred
test_set$medv
data.frame(y_pred,test_set$medv)
plot(y_pred,test_set$medv)
seq(1:72)
library(ggplot2)
library(ggplot2)
ggplot()+geom_line(aes(x=seq(1:72),y=test_set$medv),colour='red')+geom_line(aes(x=seq(1:72),y=predict(regressor,newdata = test_set)),colour='blue')+ylab("actual vs prediction")+xlab("obs")
