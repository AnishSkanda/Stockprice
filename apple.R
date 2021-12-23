
# installing/calling the required packages 
library(prophet)
library(tidyverse)
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(ggplot2)
#Pulls data from Yahoo finance till date (28/07/2021)
getSymbols("AAPL", src="yahoo", from="2016-01-01")
AAPL

#---------------------------- ARIMA

class(AAPL) #Class is xts/zoo
# Close price od the stock is taken(4th column) 1.open, 2. high, 3. Low, 4. Close, 5. Adjacent close.
AAPL_Close_Prices = AAPL[,4]  # taking the 4th column (Close_Prices)
AAPL_Close_Prices
plot(AAPL_Close_Prices) #Plotting the graph for close prices -From2 016-01-01
class(AAPL_Close_Prices)

# Checking the identifible lags by graph ACF and PCF plot PACF->p, ACF ->q for custom arima models
par(mfrow=c(1,2))

Acf(AAPL_Close_Prices,main='ACF for differenced series')
Pacf(AAPL_Close_Prices,main='Pacf for differenced seris')
# ADF test for p value.
print(adf.test(AAPL_Close_Prices)) # p value - 0.91
auto.arima(AAPL_Close_Prices,seasonal=FALSE) # Auto arima model Getting (1,1,0) with AIC=4944 , BIC = 4960.


ndiffs(AAPL_Close_Prices)
stationaryTS <- diff(AAPL_Close_Prices, differences= 1)
plot(stationaryTS, type="l", main="Differenced and Stationary")




fitA<-auto.arima(AAPL_Close_Prices,seasonal=FALSE) #fit for auto arima model seasonal is false
#time series display of residuals of fit A of lag max 40
tsdisplay(residuals(fitA),lag.max = 40,main='(1,1,0) Model Residuals') 
auto.arima(AAPL_Close_Prices,seasonal=FALSE) #AIC/BIC = 4944/4960

# lags didplayed at the 18th position from the previous fit model differencing it further for fit B a custom model
fitB<-arima(AAPL_Close_Prices,order=c(1,1,18)) 
tsdisplay(residuals(fitB),lag.max =40,main='(1,1,18) Model Residuals')

# lags didplayed at the 24th position from the previous fit model differencing it further for fit C a custom model no lags identified.
fitC<-arima(AAPL_Close_Prices,order=c(2,1,24))
tsdisplay(residuals(fitC),lag.max = 40,main='(2,1,24) Model Residuals')

#Having a set standard ARIMA model multiple lags exist 
fitD<-arima(AAPL_Close_Prices,order=c(1,1,1))
tsdisplay(residuals(fitD),lag.max = 40,main='(1,1,1) Model Residuals')


# Plot of the arima models for a total of 180 days. 3 out of the 4 models says hold.
par(mfrow=c(1,1))

term<- 180
price_forecast <- forecast(modelfit, h=term)
plot(price_forecast)

# forecasting the models with the models using the forecast function for 180 days.
fcast1<-forecast(fitA,h=term)
plot(fcast1)
fcast2<-forecast(fitB,h=term)
plot(fcast2)
fcast3<-forecast(fitC,h=term)
plot(fcast3)
fcast4<-forecast(fitD,h=term)
plot(fcast4)

# Fit B and the forecast 2 has the most accuracy among the models built.
accuracy(fcast1) # accuracy 98.7275
accuracy(fcast2) # accuracy 98.7423 
accuracy(fcast3) #accuracy 98.7321
accuracy(fcast4) # accuracy  98.7325


# Getting forecasting points the price will be 147.69$ in 180 days using the fit B model
# This also displays the high and low (80-95%) values.
forecast(fitB, h=180) 

# Getting forecasting points the price will be 162.53$ in 180 days using the fit A model
# This also displays the high and low (80-95%) values.
forecast(fitA, h=180) 

# Getting forecasting points the price will be 146.81$ in 180 days using the fit C model
# This also displays the high and low (80-95%) values.
forecast(fitC, h=180) 

# Getting forecasting points the price will be 146.94.$ in 180 days using the fit D model
# This also displays the high and low (80-95%) values.
forecast(fitD, h=180) 

# Start forecast is around 146, forecasting price will be around 147 according to the most acuurate model.
# the stats is based on 4 different arima models based on tge last 5 years of data
# the data shows a good buy for the current prices.



#Prophet Model Forecasting

#We convert dataset as prophet input requires
df <- data.frame(ds = index(AAPL),
                 y = as.numeric(AAPL[,'AAPL.Close']))
# getting a glimpse and summary of the data.
summary(df)
head(df)

#Call the prophet function to fit the model
Model1<- prophet(df) # building the model 
Future1<- make_future_dataframe(Model1,periods=180) # predicting future values
tail(Future1)

# Forecasting the models created and forecasting the data for 180 days
forecast<- predict(Model1,Future1)
forecast

# The future data is displayed with the actual value and lower and upper value
tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')])

#plot the model estimates.
dyplot.prophet(Model1,forecast)

#components of the forecast (trends with weekly and yearly seasonality)
prophet_plot_components(Model1,forecast)

#Creating train prediction datset to compare the real data
dataprediction <- data.frame(forecast$ds,forecast$yhat)
dataprediction
trainlen <- length(df)
dataprediction1 <- dataprediction[c(1:trainlen),]
dataprediction1

#Creating cross validation accuracy.
# gives an accuracy of 93.7879 %
accuracy(dataprediction$forecast.yhat,df$y)

# As we see the prices continue to increase as per the prophet model with its lower and higher values also predicted.








