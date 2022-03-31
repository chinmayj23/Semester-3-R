#install.packages("psychTools")
#install.packages("tidyverse")
library(dplyr)
library(tidyverse)
library(psychTools)
data("holzinger.raw")
data<-holzinger.raw

#CLEANING

view(data)
#Observations:
#1. Nearly 50% of last two columns are filled with NA values. The best way to deal with this is assgn a the mean of column to each null value
#2. All categorical variables are already factorized. 
#3. The numeric values are not scaled for calculation of mean, median and stdev
#Replacing na values with mean of the variable
data$t25_frmbord2[is.na(data$t25_frmbord2)]<-round(mean(data$t25_frmbord2,na.rm=TRUE))
data$t26_flags[is.na(data$t26_flags)]<-round(mean(data$t26_flags,na.rm=TRUE))

#To check for outliers, we use the z-score
z_scores <- as.data.frame(sapply(data, function(data) (abs(data-mean(data))/sd(data))))
s_data <- z_scores[!rowSums(z_scores>3), ]
dim(s_data) #There are no outliers

#Finding the mean, mode, median and std dev.

#The data is clean now, following code provides us with the mean, median and stdev of all numeric columns. 
numeric_subset<-subset(data,select = c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33))
numeric_subset
sapply(numeric_subset, function(numeric_subset) c( "Stand dev" = sd(numeric_subset), 
                         "Mean"= mean(numeric_subset,na.rm=TRUE),
                         "Median" = median(numeric_subset),
                         "mode" = max(numeric_subset)
)
)

#Simpler way, But it provides with extra information which may be unnecessary
summary(numeric_subset)

#Scaling the results of all tests from t_01 to t_26
s_data<-data %>% mutate_at(c(8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33),~(scale(.) %>% as.vector))


view(s_data)
psych::describe(data) 

#T-Test
#H0: Students of both schools have performed equally.
#Step 1: Find means of all tests for each student, to make it comparable

s_data$Means <-apply(s_data  %>% select_if(is.numeric) ,1,mean)
view(s_data)

#Create 2 different vectors for means of each school
x<-s_data$Means[c(1:156)]
y<-s_data$Means[c(157:301)]
t.test(x,y)
ttest=t.test(x,y)
names(ttest)
ttest$statistic
ts = replicate(1000,t.test(rnorm(10),rnorm(10))$statistic)
range(ts)
pts = seq(-4.5,4.5,length=100)
plot(pts,dt(pts,df=18),col='red',type='l')
lines(density(ts))
#Since the p-value is significantly lower than 0.05, we reject the H0. Thus, students of both schools have not performed equally in the tests.