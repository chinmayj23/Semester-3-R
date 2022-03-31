a<-read.csv("C:/Users/voraa/Desktop/2-way anova.csv")
a$gender<-as.factor(a$gender)
a$age.group<-as.factor(a$age.group)
anova<-aov(score~gender*age.group,data=a)
summary(anova)
TukeyHSD(anova)
#casestudy
library(ISLR)
Default
str(Default)
anova1<-aov(income~default*student,data=Default)
summary(anova1)
TukeyHSD(anova1)
