Hitters
#ridge regression

library(ISLR)

Hitters

sum(is.na(Hitters))

names(Hitters)

Hitters=na.omit(Hitters)

install.packages("glmnet")

library(glmnet)

X=model.matrix(Salary~.,Hitters)[,-1]

Y=Hitters$Salary

ld=10^seq(10,-2,length =100) #user defied lambda

reg.fit<-glmnet(X,Y,alpha = 0) #if alpha=0 then ridge if alpha =1 then lasso

reg.fit1<-glmnet(X,Y,alpha = 0,lambda = ld)  ##user define lambda

plot(reg.fit)

plot(reg.fit1)

cv.reg.fit=cv.glmnet(X,Y,alpha=0,mfold=5)  ##10 fold cross validation, we can change it by nfold

plot(cv.reg.fit)

cv.reg.fit$lambda.min



##user define lambda

cv.reg.fit1=cv.glmnet(X,Y,alpha=0,lambda = ld)

plot(cv.reg.fit1)

k=cv.reg.fit1$lambda.min

out=glmnet(X,Y,alpha=0)

predict(out,type="coefficients",s=k)[1:20,]



##split the data set

set.seed(1)

train_set=sample(1:nrow(X),nrow(X)/2)

test_set=(-train_set)

y_test=Y[test_set]



reg.fi1<-glmnet(X[train_set,],Y[train_set],alpha = 0)

plot(reg.fi1)

cv.out1=cv.glmnet(X[train_set,],Y[train_set],alpha=0)

plot(cv.out1)

bestlambda=cv.out1$lambda.min

ridge.pred1=predict(reg.fi1,s=bestlambda,newx=X[test_set,])

mean((ridge.pred1-y_test)^2)

predict(reg.fi1,type="coefficients",s=bestlambda)[1:20]

#lasso regression

library(ISLR)

Hitters

sum(is.na(Hitters))

Hitters=na.omit(Hitters)

names(Hitters)

Hitters=na.omit(Hitters)

install.packages("glmnet")

library(glmnet)

X=model.matrix(Salary~.,Hitters)[,-1]

Y=Hitters$Salary

ld=10^seq(10,-2,length =100)

reg.fit<-glmnet(X,Y,alpha = 1)

reg.fit1<-glmnet(X,Y,alpha = 1,lambda = ld)

plot(reg.fit)

plot(reg.fit1)

cv.reg.fit=cv.glmnet(X,Y,alpha=1)  ##10 fold cross validation, we can change it by n fold

plot(cv.reg.fit)

cv.reg.fit$lambda.min



cv.reg.fit1=cv.glmnet(X,Y,alpha=1,lambda = ld)

plot(cv.reg.fit1)

k=cv.reg.fit1$lambda.min

out=glmnet(X,Y,alpha=1,lambda = ld)

predict(out,type="coefficients",s=k)[1:20,]



##split the data set

set.seed(1)

train_set=sample(1:nrow(X),nrow(X)/2)

test_set=(-train_set)

y_test=Y[test_set]



reg.fi1<-glmnet(X[train_set,],Y[train_set],alpha = 1)

plot(reg.fi1)

cv.out1=cv.glmnet(X[train_set,],Y[train_set],alpha=1)

plot(cv.out1)

bestlambda=cv.out1$lambda.min

ridge.pred

ridge.pred1=predict(reg.fi1,s=bestlambda,newx=X[test_set,])

mean((ridge.pred1-y_test)^2)


Attachments area
