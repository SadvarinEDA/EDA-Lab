install.packages("GGally")
library(class)
library(caTools)

data(iris)
summary(iris)

splitd<-sample.split(iris,SplitRatio = 0.8)
train <- subset(iris,splitd=="TRUE") 
test <- subset(iris,splitd=="FALSE") 
View(train)
View(test)

norm<- function(x){((x-min(x))/(max(x)-min(x)))}
norm_train <- as.data.frame(lapply(train[,1:4],norm))
norm_test <- as.data.frame(lapply(test[,1:4],norm))
View(norm_test)

pred<-knn(train = norm_train, test = norm_test, cl = train$Species,k=5)
cf <- table(test$Species,pred)
cf

ACC <- (cf[[1,1]]+cf[[2,2]]+cf[[3,3]])/sum(cf)
ACC
