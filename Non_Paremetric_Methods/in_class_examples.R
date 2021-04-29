library(TTR)
library(forecast)

kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3) 
kingstimeseries <- ts(kings)

plot(kingstimeseries)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat") 
birthstimeseries <- ts(births, frequency=12, start=c(1946,1)) 
plot(birthstimeseries)

souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat") 
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1)) 
plot(souvenirtimeseries)
plot(log(souvenirtimeseries))

kingstimeseriesSMA3 <- SMA(kingstimeseries,n=3) 
plot.ts(kingstimeseriesSMA3) 

kingstimeseriesSMA10 <- SMA(kingstimeseries,n=10) 
plot.ts(kingstimeseriesSMA10) 
kingstimeseries

birthstimeseriescomponents <- decompose(birthstimeseries) 
plot(birthstimeseriescomponents)

birthstimeseriesseasonallyadjusted <- birthstimeseries - birthstimeseriescomponents$seasonal
plot(birthstimeseriesseasonallyadjusted)

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1) 
rainseries <- ts(rain,start=c(1813))
plot(rainseries)
 
rainseriesforecasts <- HoltWinters(rainseries, beta=FALSE, gamma=FALSE) 
plot(rainseriesforecasts)


skirts <- scan("http://robjhyndman.com/tsdldata/roberts/skirts.dat",skip=5) 
skirtsseries <- ts(skirts,start=c(1866)) 
skirtsseriesforcasts <- HoltWinters(skirtsseries, gamma=FALSE)
plot(skirtsseries)
plot(skirtsseriesforcasts)
skirtsseriesforecasts2 <- forecast(skirtsseriesforcasts, h=5)
plot(skirtsseriesforecasts2)

acf(skirtsseriesforecasts2$residuals[-c(1,2)], lag.max=20)
Box.test(skirtsseriesforecasts2$residuals[-c(1,2)], lag=20)




souvenir <- scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirtimeseries <- ts(souvenir, frequency=12, start=c(1987,1)) 
souvenirtimeseries 
plot(souvenirtimeseries) 
logsouvenirtimeseries <- log(souvenirtimeseries) 
plot(logsouvenirtimeseries) 
log_svnr_ts_smth <-  HoltWinters(logsouvenirtimeseries)
log_svnr_ts_smth
plot(log_svnr_ts_smth)
#forecast
log_svnr_ts_fc <- forecast(log_svnr_ts_smth, h=60)
plot(log_svnr_ts_fc)
log_svnr_ts_fc$residuals
acf(log_svnr_ts_fc$residuals[-c(1:12)], lag.max=20)
Box.test(log_svnr_ts_fc$residuals, lag=20, type='Ljung-Box')
