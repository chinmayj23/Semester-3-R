a<-read.csv("C:/Users/voraa/Desktop/churn.csv")
a
lg.fit<-glm(churn~.,data = a,family="binomial")
lg.fit
summary(lg.fit)
sum(is.na(a))
library(boot)
lg.fit1<-glm(churn~reside+tollfree+callcard+tollmon+cardmon+tollten+internet,data = a,family = "binomial")
summary(lg.fit1)
lg.fit1<-glm(churn~tollfree+callcard+tollten+internet,data = a,family = "binomial")
summary(lg.fit1)
