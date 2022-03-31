library(ISLR)
#install.packages("pscl")
library(pscl)


Default
str(Default)
model1 <- glm(default~balance,data=Default,family="binomial")
summary(model1)
model1_AIC = 2920.7

model2 <- glm(default~income+balance,data = Default,family="binomial")
summary(model2)
model2_AIC = 1585

# Model 2 is better than model 1 as AIC is smaller for model 2
# Hence, smaller the AIC better the model is 

model3 <- glm(default~income+balance,data = Default,family="binomial")
summary(model3)

anova(model1,model2,test="Chisq")
# Model2 is better than Model1 as the p value is less than 0.05


# Pseudo R2
# Here we consider the higher value 
# model which has the higher value of R2 is a better model

list(model1 = pscl::pR2(model1)["McFadden"],
     model2 = pscl::pR2(model2)["McFadden"],
     model3 = pscl::pR2(model3)["McFadden"])



# For Multi logit use library nnet

#install.packages("nnet")
library(nnet)

# for data sets rattle.data
# install.packages("rattle.data")

library(rattle.data)

data(wine)

str(wine)

# take a reference i.e to comapare one with the rest

wine$Type <- relevel(wine$Type,ref = "3")
wine

# for multi logit we use multinom

multinon.fit <- multinom(Type~Alcohol+Color,data = wine)
summary(multinon.fit)
#for removing the intercept we write multinom(Type~Alcohol+Color -1,data=wine)

#so here we have 2 models where in model1  wine$Type 3 is compared with wine$Type 2
#and in model2 wine$Type 3 is compared with wine$Type 1

head(probability.table) <- fitted(multinon.fit)

wine$predicted <- predict(multinon.fit,newdata = wine,type = "class")

# Buildding a classification table

ctable <- table(wine$Type,wine$predicted)
ctable

############################# IRIS  data set

iris
str(iris)

multinom.fit1 <- multinom(Species~.-1,data = iris)
summary(multinom.fit1)


head(probability.table) <- fitted(multinom.fit1)

iris$predicted <- predict(multinom.fit1,newdata = iris,type = "class")

# Buildding a classification table

ctable1 <- table(iris$Species,iris$predicted)
ctable1


