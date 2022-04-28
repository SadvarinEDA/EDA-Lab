library(forecast)
library(tseries)

### GOLD

gold<-read.csv("gold.csv")

gold_ts<-ts(gold$Price,start = min(gold$Month),end=max(gold$Month),frequency = 1)
class(gold_ts)
plot(gold_ts)

acf(gold_ts)
pacf(gold_ts)

adf.test(gold_ts)
gold_model=auto.arima(gold_ts,ic="aic",trace = TRUE)

gold_f = forecast(gold_model,level = c(95),h=24)
gold_f
plot(gold_f)


### GDP

gdp<-read.csv("gdp.csv")

gdp_ts<-ts(gdp$GDP_gr,start = min(gdp$Year),end=max(gdp$Year),frequency = 1)
class(gdp_ts)
plot(gdp_ts)

acf(gdp_ts)
pacf(gdp_ts)

adf.test(gdp_ts)
gdp_model=auto.arima(gdp_ts,ic="aic",trace = TRUE)

gdp_f = forecast(gdp_model,level = c(95),h=10)
gdp_f
plot(gdp_f)

