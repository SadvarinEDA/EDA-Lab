setwd("E:/alishma/ACADEMICS/STUDY MATERIAL SEM 6/A2_EDA_CSE3506/LAB")
df<-read.csv("weatherHistory2016.csv")

library(dplyr)
library(corrplot)
library(forecast)
library(tseries)

head(df)

ml1<-lm(dt$df.Temperature..C. ~ dt$df.Apparent.Temperature..C. + dt$df.Humidity + dt$df.Wind.Speed..km.h.+dt$df.Wind.Bearing..degrees.+dt$df.Visibility..km.,data = dt)
summary(ml1)

ml2<-lm(dt$df.Temperature..C. ~ dt$df.Apparent.Temperature..C. + dt$df.Humidity +dt$df.Wind.Speed..km.h.+dt$df.Wind.Bearing..degrees.,data =dt)
summary(ml2)

dt<-data.frame(df$Temperature..C.,df$Apparent.Temperature..C.,df$Humidity,df$Wind.Speed..km.h.,df$Wind.Bearing..degrees.)

corr<-cor(dt)
corrplot(corr)

### MODEL 2 - FORCASTING

date<-df$Formatted.Date
dt<-cbind(dt,date)
head(dt)
str(dt)

dt<-na.omit(dt)

tseries<-ts(dt$df.Temperature..C.,start = as.Date("2016-01-01 00:00"),end=as.Date("2016-12-31 22:59"),frequency = 24)
plot(tseries)

acf(tseries)
pacf(tseries)

adf.test(tseries)
model=auto.arima(tseries,ic="aic",trace = TRUE)

forc = forecast(model,level = c(95),h=24)
forc
plot(forc)

