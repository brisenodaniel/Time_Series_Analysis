library(forecast)
#Problem 2
#part a
set.seed(1224)
ts=arima.sim(n=200, list(order=c(1,3,1), ar=0.6, ma=-0.7, sd=sqrt(10))) + 100
plot(ts)

ts


ndiffs(ts)
auto.arima(ts)
acf(ts)
acf(diff(ts))
acf(diff(diff(ts)))
acf(diff(diff(diff(ts))))

#part b 
arima(ts, order=c(1,3,1), method='ML')
arima(ts, order=c(1,3,1), method='CSS')
arima(ts, order=c(1,3,1), method='CSS-ML')


#question 4
library(tidyquant)
getSymbols('AAPL', from='2020-04-27', to='2021-04-27')
head(AAPL)
#a
ts.plot(AAPL$AAPL.Close)
ts = AAPL$AAPL.Close
# Stocks went up, this was a good year for apple. Mean is moving denoting
# that this is likely not stationary

#b
library(aTSA)
adf.test(ts)

#c
auto.arima(ts)
fit = auto.arima(ts)
names(fit)
Box.test(fit$residuals)
Box.test(fit$residuals, type='Ljung-Box')
