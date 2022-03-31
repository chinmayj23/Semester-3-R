b<-read.csv("C:/Users/voraa/Desktop/championsdata.csv")
names(b)
str(b)
dim(b)
sum(is.na(b))
a<-na.omit(b)
str(a)
a$Game<-as.factor(a$Game)
a$FGP<-scale(a$FGP)
a$TPP<-scale(a$TPP)
a$FTP<-scale(a$FTP)
a$ORB<-scale(a$ORB)
a$DRB<-scale(a$DRB)
a$AST<-scale(a$AST)
a$STL<-scale(a$STL)
a$BLK<-scale(a$BLK)
a$TOV<-scale(a$TOV)
a$PF<-scale(a$PF)
a$PTS<-scale(a$PTS)
library(caTools)
library(glmnet)
set.seed(123)
split = sample.split(a$Win,SplitRatio = 0.6)
train = subset(a,split == T)
test = subset(a,split == F)
Y = a$Win
X = model.matrix(Win~.,a)[,-1]
l1 <- glmnet(X,Y,alpha = 1)
X
plot(l1)
l1.cv <- cv.glmnet(X,Y,alpha = 1,nfolds = 10)
plot(l1.cv)
lambdamin = l1.cv$lambda.min
predict(l1,type="coefficients",s=lambdamin)[1:40,]
m1 <- glm(Win~.-Year-Home-minutes-FG-TP-FTA-ORB-BLK-AST-PTS,data = train,family = "binomial")
m1
summary(m1)
pred <- predict(m1,newdata = test,type = "response")
pred
pred_win <- ifelse(pred>0.5,1,0)
pred_win
table(test$Win,pred_win)
m2 <- glm(Win~.-Year-Home-minutes-FG-TP-FTA-ORB-BLK-AST-PTS-TPA-FT-DRB-PF-Team,data = train,family = "binomial")
summary(m2)
pred1 <- predict(m2,newdata = test,type = "response")
pred1
pred_win1 <- ifelse(pred1>0.5,1,0)
pred_win1
table(test$Win,pred_win1)

