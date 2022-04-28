#Lab 1 Linear Regression
#threshold == 0.05

# 1.Importing required libraries
# 2.Reading/Taking input dataset
# 3.Splitting the data into training and testing dataset
# 4.Plot the scatterplot between the independent and dependent variable
# 5.Check the correlation between the variables
# 6.Using the "lm" inbuilt function, train a linear regression model.
# 7.Use the summary function to get the statistics of the model created
# 8.Using abline function plot the trained linear model on scatterplot

library(corrplot)
library(dplyr)
data <- mtcars
corrplot(data)
df_train <- sample_n(data, 15)
plot(df_train$wt,df_train$mpg,main = 'scatter plot', xlab = "weight", ylab = "mpg" )
M<-cor(data)
M
cor.test(df_train$wt,df_train$mpg)
cor.test(df_train$disp,df_train$mpg)
corrplot(M, method="circle") 
model_1 <- lm(mpg~wt, data = df_train)
summary(model_1)
abline(model_1,col="blue")
