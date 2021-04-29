#Q2
library(forecast)
set.seed(1224)
ts = arima.sim(list(order=c(1,3,1), ma=-0.7, ar=0.6), sd=sqrt(10), n=200) + 100
ts

ts_mod = auto.arima(ts)
ts_mod

#b

#using CSS method
arima(ts, order=c(2,2,0), method='CSS')
#using unconditional least squares
arima(ts, order=c(2,2,0), method = 'CSS-ML')
#using Maximum Likelihood
arima(ts, order=c(2,2,0), method='ML')


#Q4

#a
library(quantmod)
getSymbols('AAPL')
head(AAPL)
ts = AAPL$AAPL.Close

plot(ts)
#Data does not appear stationary. The mean increases over time and it increases
# at faster rates as time goes on, indicating that variance is not constant

#b Perform a test for stationarity
library(tseries)
adf.test(ts)
#The output strongly suggests that the null hypothesis holds and the data is not 
#stationary
ndiffs(ts)
adf.test(na.omit(diff(diff(ts))))
#after differencing twice, null hypothesis is rejected with p value of 0.01. Thus
# the twice differenced dataset is probably stationary

#c Find the best fitting ARIMA Model for these data
mod = auto.arima(ts)
mod
#best fitting model is ARIMA(5,2,0) 

#d Find the parameters of the best ARIMA model using ML and parametric bootstrapping
mod2 = auto.arima(ts, method='ML')
mod2 #best model using ML

#e Use Ljung-Box and Box-Pierce tests for independence of the residuals and comment

Box.test(mod2$residuals, type="Box-Pierce")
# we cannot conclude IID residuals based off of this test with an alpha of 0.05
Box.test(mod2$residuals, type='Ljung-Box')
#Here we again cannot conclude IID residuals based off of this test with an alpha
# of 0.05.


