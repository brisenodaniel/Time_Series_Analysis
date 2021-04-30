library(forecast)
library(tidyquant)
library(aTSA)
library(TSA)
#QUESTION 2
set.seed(2254)
ts = arima.sim(n=500, model=list(order=c(2,1,2), ar=c(0.6,-0.2), ma=c(-0.7,-01)),sd=sqrt(6)) + 22
ts

# part a
plot(ts) #model looks approximately stationary, but mean may move around
plot(diff(ts)) #model looks much more stationary here, with constant mean
ndiffs(ts)# output of ndiffs suggests that model is stationary after differencing once,
#           confirming suspicions from looking at the plots
eacf(diff(ts)) #extended autocorrelation function plot suggests ARIMA(0,1,4)
auto.arima(ts) #auto arima agrees with our predictions using eacf

#part b
#ML estimation
mod_ML = auto.arima(ts, method='ML')
#CLS estimation
mod_CLS = auto.arima(ts, method='CSS')
#UCLS estimation
mod_UCL = auto.arima(ts, method='CSS-ML')

mod_ML
mod_CLS
mod_UCL
#Surprisingly, in this case, CLS seems to give the parameters with the best p-values

#Problem 4
getSymbols('GOOGL', from='2020-04-29', to='2021-04-29')
names(GOOGL)
plot(GOOGL$GOOGL.Close)
ts = GOOGL$GOOGL.Close
#Data is not stationary, with an increasing mean. Sudden jumps in stock price 
#indicate that autocorrelation might not be only dependent on distance between points,
#but the entire trend looks approximately linear, so the only violation of stationarity
#might be a moving mean.
adf.test(ts)
#adf test strongly indicates that the data is NOT stationary.
ndiffs(ts)
plot(diff(ts))
#data now looks approximatley stationary, but still has large jumps
adf.test(na.omit(diff(diff(ts))))
#adf still strongly suggests data is not stationary
acf(na.omit(diff(ts)))
acf(na.omit(diff(diff(ts))))
acf(na.omit(diff(diff(diff(ts)))))
#taking increasing differences shows that data continues to have non-zero acf
#values for large lags. Again, this is a sign of an explosivley non-stationary dataset
#however, after taking one difference, data appears nearly stationary, and might
#be suitably approximated by a ARIMA(p,1,q) model

#d
eacf(na.omit(diff(ts)))
#extended autocorrelation function suggests ARIMA(0,1,1)
auto.arima(ts)
#auto arima finds a model ARIMA(0,1,0) with drift
# , which matches the appearance of the plot for a single difference

names(arima(ts, order=c(0,1,1)))
mod1 = arima(ts, order=c(0,1,1))
mod2 = auto.arima(ts)
mean(mod1$residuals)
mean(mod2$residuals)
#the auto arima model seems to be best
mod = auto.arima(ts)

#d
mod = auto.arima(ts, method='ML')

#e
Box.test(mod$residuals, type=c('Box-Pierce'))
Box.test(mod$residuals, type=c('Ljung-Box'))
#Both tests fail to reject, thus residuals are IID and the model is an acceptable fit.
