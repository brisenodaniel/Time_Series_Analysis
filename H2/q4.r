library(tidyquant)
library(forecast)
getSymbols("MRNA", from = '2020-03-08', to = "2021-03-08",warnings = FALSE, auto.assign = TRUE)
# Time series graph with forecst
plot(MRNA$MRNA.Close)
model=auto.arima(MRNA$MRNA.Close)
forecast(model,10)
plot(forecast(model,10))
# Candlestick graphs
 chart_Series(MRNA,name="Moderna")
 chart_Series(MRNA,name="Moderna",subset="2021-01/2021-03") 
