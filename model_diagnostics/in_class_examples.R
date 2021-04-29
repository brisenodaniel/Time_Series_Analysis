library(TSA)

data(oil.price)
plot(oil.price)

plot(log(oil.price))

plot(diff(log(oil.price)))

oil_price_stationary = diff(log(oil.price))

acf(oil_price_stationary)
pacf(oil_price_stationary)


#thus we hypothessise ARIMA(0,0,1)

arima(oil_price_stationary, order=c(0,0,1), method="ML")

#using CSS to show how its not as good as ML
arima(oil_price_stationary, order=c(0,0,1), method="CSS")

mod_1 = arima(oil_price_stationary, order=c(0,0,1), method="ML")

acf(residuals(mod_1))
Box.test(residuals(mod_1))
