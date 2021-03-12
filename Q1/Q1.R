library(tidyquant)
library(forecast)
library(rlist)
# A.)
getSymbols('PFE',from = '2019-03-11', to = "2021-03-11",warnings = FALSE, auto.assign = TRUE)


# B.)
plot(PFE$PFE.Close)
# The time series looks like it might have a constant, since large fluctuations upwards
#   are compensated by large fluctionations downwards at different times. However,
#   an argument for non-constant mean could be made by noting that the first five months
#   of data seem to have a higher mean than the rest of the data

#As far as the autocorrelation, there seems to be large fluctuations in variance
# over time, with some periods experiencing relatively low variance, such as the first
# four months of data, and some experiencing a large negative or positive autocorrelation.
# For example, Aug 2019 experienced a large autocorrelation.

# For this reason, I don't believe this data is stationary. However, there does not 
# seem to be any explosive growth, thus I believe the data to be a good candiate for 
# differencing


#C.
fit_pfe <- auto.arima(PFE$PFE.Close)
forecast_pfe <- forecast(fit_pfe,20)
autoplot(forecast_pfe)

#The forcast deems that the stock prices will most likely not change over the next
# 20 days, but it also provides a large cone of uncertainty around its prediction,
# indicating that the stock price will not be very stable. However, the stock prices will very
# likey be within 40 and -40 in the next 20 days.