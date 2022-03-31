install.packages("caret")
library(caret)
data(GermanCredit)
sum(is.na(GermanCredit))
library(caTools)
set.seed(1)
a<-GermanCredit
names(a)
split<-sample.split(a$Class,SplitRatio = 0.6)
train<-subset(a,split==T)
test<-subset(a,split==F)
lg.fit<-glm(Class~Age+ForeignWorker+Property.RealEstate+Housing.Own+CreditHistory.Critical,data=train,family="binomial")
summary(lg.fit)
exp(coef(lg.fit))
y_predict<-predict(lg.fit,newdata= test,type="response")
y_predict
1/(1+exp(0.879633573))
y_predict1<-ifelse(y_predict>0.5,"good","bad")
y_predict1
table(y_predict1,test$Class)
(268+12)/400
#cross validation in logistic reg
library(e1071)
install.packages("e1071") 
trc<-trainControl(method="repeatedcv",number=10,savePredictions = TRUE)
mod_fit<-train(Class~Age+ForeignWorker+Property.RealEstate+Housing.Own+CreditHistory.Critical,data=train,family="binomial",trControl=trc)
pred=predict(mod_fit,newdata = test)
confusionMatrix(data=pred,test$Class)
