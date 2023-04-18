library(smooth)
# SMA of specific order
ourModel <- sma(rnorm(118,100,3),order=12,h=18,holdout=TRUE,interval="p")

# SMA of arbitrary order
ourModel <- sma(rnorm(118,100,3),h=18,holdout=TRUE,interval="sp")

summary(ourModel)
forecast(ourModel)
plot(forecast(ourModel))


y <- csvs[[1]][-2349,2]
tail(csvs[[1]][-2349,1])
ymodel <- sma(y,h=60)
forecast(ymodel,400)
plot(forecast(ymodel,60))

fit <- ets(diff(y,lag=12),model="ZZA")

fit <- forecast::Arima(y,order=c(1,1,0))
forecast::forecast(fit,h=400)
plot(forecast::forecast(fit,h=400))

forecast::stlf(y)
autoplot(y)
library(forecast)
autoplot(as.numeric(y))
plot(y)
plot(diff(y))
autoplot(diff(ts(y)))
autoplot(ts(y))
autoplot(diff(ts(y),lag=12))         
y.df <- diff(ts(y),lag=12)

auto.arima(y.df)
autoplot(forecast(auto.arima(y.df)),h=200)
forecast(auto.arima(y.df),h=20)

autoplot(y.df)+
  xlim(c(2000,2500))

library(fpp2)
fit <- nnetar(y,p=3,P=1)
autoplot(forecast(fit,h=600))

autolayer(forecast(auto.arima(y.df),h=200)+
              xlim=c(2000,2500)
            
accuracy(test,testhat)            
test <- as.numeric(test.true1[,2])
testhat <- forecast(fit,h=64)$mean
testhat <- as.numeric(testhat)

forecast(prison.gts, method="bu", fmethod="arima")


hts(y,characters=3)
library(hts)
prison
prison.gts <- gts(prison/1e3, characters = c(3,1,9),
                  gnames = c("State", "Gender", "Legal",
                             "State*Gender", "State*Legal",
                             "Gender*Legal"))
