library(caTools)
library(dplyr)

mydata<-read.csv("Social_Network_Ads.csv")
glimpse(mydata)

splitd<-sample.split(mydata,SplitRatio = 0.8) 
train=subset(mydata,splitd=="TRUE") 
test=subset(mydata,splitd=="FALSE")

mydata$Gender<-as.factor(mydata$Gender) 
mydata$Purchased<-as.factor(mydata$Purchased)

mymodel <- glm(Purchased ~ Age+Gender+EstimatedSalary, data=train, family='binomial')
summary(mymodel)

restest<-predict(mymodel,test,type='response')
cfmatrix<-table(Act=test$Purchased, pred=restest>0.5) 
cfmatrix 

Acc=(cfmatrix[[1,1]]+cfmatrix[[2,2]])/sum(cfmatrix) 
Acc