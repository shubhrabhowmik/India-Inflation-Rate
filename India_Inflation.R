# INFLATION RATE
# A measure of change in purchasing power per unit of money.

library(forecast)
mydata = scan()
india_inflation = ts(mydata, start=2008, frequency = 12)
plot(india_inflation)

# Learn about Seasonality and Cross validation
# Inflation has no trend, and has seasonal patterns
# So we will use 'Seasonal Decomposition', 'Exponential Smoothing', 
# 'Seasonal ARIMA' and 'Cross Validation'(for comparing) the models


#decompose()- no forecast available
#STL decomposition with stl()
#Combination of STL + ETS stlf()
#ets()- automated / hw()
#Cross validation with tsCV()

# Seasonal Decomposition
decompose(india_inflation)
plot(decompose(india_inflation))

# Using the stl method
plot(stl(india_inflation, s.window = 7))

# STL forecasting
plot(stlf(india_inflation, method="ets"))

# Comparision with a standard ets forecast
plot(forecast(ets(india_inflation),h=24))

# Using autoplot
library(ggplot2)
autoplot(stlf(india_inflation, method='ets'))

# Seasonal Arima (package forecast)
auto.arima(india_inflation, stepwise = T, approximation = F, trace = T)

# Getting an object
infla_ind = auto.arima(india_inflation, stepwise = T, approximation = F, trace = T)

# Forecast
forc = forecast(infla_ind)
plot(forc)

## Exponential Smoothing with ets
# Auto generated
ets(india_inflation)

# Forecast plot
plot(forecast(ets(india_inflation),h=60))

# Comparison with seasonal Holt Winters model
plot(hw(india_inflation, h=60))

# Cross Validation of 2 model
inflation_ets = ets(india_inflation)
auto.arima(india_inflation, stepwise = T,
           approximation = F, trace = T)

forecastets = function(x,h){
  forecast(ets(x),h=h)
}

forecastarima = function(x,h){
  forecast(auto.arima(x), stepwise=T,
           approximation=F, h=h)
}

etserror = tsCV(india_inflation, forecastets, h=1)
arimaerror = tsCV(india_inflation, forecastarima, h=1)

mean(etserror^2, na.rm=T)
mean(arimaerror^2, na.rm=T)

