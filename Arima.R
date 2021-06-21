library(googlesheets4)
mt<-data.frame(read_sheet("https://docs.google.com/spreadsheets/d/1Tab1mVUngPDxPhenDO9N5tTODzXUnGioOo4DNw8b_zQ/edit#gid=0"))

library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)

getSymbols('AUROPHARMA.NS', from='2014-01-01', to='2020-05-29')
stock_prices = AUROPHARMA.NS[,4]
stock = diff(log(stock_prices),lag=1)
stock = stock[!is.na(stock)]

# Plot log returns 
plot(stock,type='l', main='log returns plot')
# Conduct ADF test on log returns series
print(adf.test(stock))

# Split the dataset in two parts - training and testing
breakpoint = min(grep("2020-05", index(stock)))-1

# Apply the ACF and PACF functions
par(mfrow = c(1,1))
acf.stock = acf(stock[c(1:breakpoint),], main='ACF Plot', lag.max=100)
pacf.stock = pacf(stock[c(1:breakpoint),], main='PACF Plot', lag.max=100)


# Initialzing an xts object for Actual log returns
Actual_series = xts(0,as.Date("2020-05-04","%Y-%m-%d"))

# Initialzing a dataframe for the forecasted return series
forecasted_series = data.frame(Forecasted = numeric())
for (b in breakpoint:(nrow(stock)-1)) {
  stock_train = stock[1:b, ]
  stock_test = stock[(b+1):nrow(stock), ]
  # Summary of the ARIMA model using the determined (p,d,q) parameters
  fit = arima(stock_train, order = c(2, 0, 2),include.mean=FALSE)
  summary(fit)
  # plotting a acf plot of the residuals
  acf(fit$residuals,main="Residuals plot")
  # Forecasting the log returns
  arima.forecast =  forecast(fit, h = 1,level=99) ##forecast.Arima(fit, h = 1,level=99)
  summary(arima.forecast)
  # plotting the forecast
  par(mfrow=c(1,1))
  plot(arima.forecast, main = "ARIMA Forecast")
  # Creating a series of forecasted returns for the forecasted period
  forecasted_series = rbind(forecasted_series,arima.forecast$mean[1])
  colnames(forecasted_series) = c("Forecasted")
  # Creating a series of actual returns for the forecasted period
  Actual_return = stock[(b+1),]
  Actual_series = c(Actual_series,xts(Actual_return))
  rm(Actual_return)
  print(stock_prices[(b+1),])
  print(stock_prices[(b+2),])
}

# Adjust the length of the Actual return series
Actual_series = Actual_series[-1]
# Create a time series object of the forecasted series
forecasted_series = xts(forecasted_series,index(Actual_series))
# Create a plot of the two return series - Actual versus Forecasted
plot(Actual_series,type='l',main='Actual Returns Vs Forecasted Returns')
lines(forecasted_series,lwd=1.5,col='red')
legend('bottomright',c("Actual","Forecasted"),lty=c(1,1),lwd=c(1.5,1.5),col=c('black','red'))
# Create a table for the accuracy of the forecast
comparsion = merge(Actual_series,forecasted_series)
comparsion$Accuracy = sign(comparsion$Actual_series)==sign(comparsion$Forecasted)
print(comparsion)
# Compute the accuracy percentage metric
Accuracy_percentage = sum(comparsion$Accuracy == 1)*100/length(comparsion$Accuracy)
print(Accuracy_percentage)