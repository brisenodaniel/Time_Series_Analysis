library(TSA)
library(forecast)
data(co2)
plot(co2)
acf(as.vector(co2))
acf(diff(co2), lag.max=36)
plot(diff(co2))

#plot seasonal difference 
plot(diff(diff(co2), lag=12))
acf(as.vector(diff(diff(co2),lag=12)),lag.max=36,ci.type='ma')
fit = arima(co2, order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
fit
hist(fit$residuals)
qqnorm(fit$residuals)
acf(fit$residuals)
#must analyze last n-12 items
Box.test(fit$residuals, lag=10, fitdf=2)

plot(fit, n.ahead=84)

plot(fit,n.ahead=36)
plot(fit, n.ahead=400)
