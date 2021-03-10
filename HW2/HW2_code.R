library(tidyquant)
library(forecast)
library(rlist)
# A.)
getSymbols('ILMN',from = '2015-03-08', to = "2021-03-08",warnings = FALSE, auto.assign = TRUE)
getSymbols('TSLA',from = '2015-03-08', to = "2021-03-08",warnings = FALSE, auto.assign = TRUE)

# B.)
plot(TSLA$TSLA.Close)
plot(ILMN$ILMN.Close)

#the Tesla stocks seemed stationary from 2015 to 2019, however, beginning
# in late 2019 there seemed to be explosive growth in the stock price, and 
# the mean past that point is definitley not constant. Additionally after 
# that point there seems to be much more variance in the price, indicating that
# the autocovriance is no longer small as the it appeared to be in the years 
# 2015-2019

#The Illumina stock price does not appear stationary in any time-period. The 
# mean is most definitley not constant, since we see that the stock prices 
# have an upward trend, and there appears to be increasingly positive
# autocorrelation as the time increases.

# C.)
fit_ILMN <- auto.arima(ILMN$ILMN.Close)
fit_TSLA <- auto.arima(TSLA$TSLA.Close)

TSLA['2015-03/2015-07']
TSLA2 <- TSLA['2015-03/2015-07']

# divide time-series data into 4-month increments
make_date_bounds <- function(dates){ #helper function
  upper_d = dates$upper
  lower_d = dates$lower
  paste(lower_d,upper_d,sep='/')
}

#make list of data bounds in "lower_date/upper_date" format
lower = seq(d,by=120, to=as.Date('2021-03-05'))[-19]
upper = seq(d,by=120, to=as.Date('2021-03-05'))[-1]
bounds = list.zip(lower,upper)
bounds = lapply(bounds,make_date_bounds)

#fit ARIMA models to 4-month windows
fit_ARIMA <- function(bound, data) auto.arima(data[bound][,4])
fit_TSLA <- function(bound) fit_ARIMA(bound,TSLA)
fit_ILMN <- function(bound) fit_ARIMA(bound,ILMN)

tesla_models <- lapply(bounds,fit_TSLA)
ilmn_models <- lapply(bounds,fit_ILMN)

#print ARIMA models
tesla_models
ilmn_models

# The most common model for TSLA seems to be ARIMA(0,1,0), or a model which
# becomes a random walk after a single difference

# The most common model for ILMN seems to be ARIMA(0,1,0) again, so it seems 
# that the simplest ARIMA model tends to happen with highest frequency


# D.)
forecast_12 = function(mod) forecast(mod,12)

tesla_forecasts = lapply(tesla_models, forecast_12)
lapply(tesla_forecasts,plot)

ilmn_forecasts = lapply(ilmn_models,forcast_12)
lapply(ilmn_forecasts,plot)

#forecasting on full data

tesla_full_model = auto.arima(TSLA$TSLA.Close)
ilmn_full_model = auto.arima(ILMN$ILMN.Close)
plot(forecast(tesla_full_model,12))
plot(forecast(ilmn_full_model,12))
