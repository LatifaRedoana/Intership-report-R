library(xts)


#loading the daily_means data file
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx') 
View(daily_means)

# select column for solar installed
library(dplyr)
daily_totalpower<- daily_means%>%select(date,`Power.retrieved.total`)
head(daily_totalpower,5)


# declare data as time series data
Totalpower_ts<-ts(daily_totalpower$Power.retrieved.total , start=c(2015,1), frequency=365)
adf.test(Totalpower_ts)

#Time series plot for Total power retrieved  energy
library(fpp2)
autoplot(Totalpower_ts)+ylab("Totalpower")+xlab("year")+geom_line(col='cyan4') +
  theme_minimal() + 
  ggtitle("Daily Total power retrieved (Past 8 years)") +
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))

Totalpower_train <- window(Totalpower_ts, start=c(2015,1), end=c(2021,127))
Totalpower_test<-window(Totalpower_ts, start=c(2021,128),end=c(2022,365))
#For daily data we have frequency 365 and we can not model the series arima function since the R software does not support frequency more than 350.The Arima() and auto.arima() functions will allow a seasonal period up to m=350.
#An alternative approach is to use a dynamic harmonic regression model, the number of Fourier terms was selected by minimising the AICc. The order of the ARIMA model is also selected by minimising the AICc, although that is done within the auto.arima() function.


##Applying fourier dynamic regreesion model for frequency 365 with order of regressors, k=5-10 (the number of Fourier sin and cos pairs)
# # Set up harmonic regressors of order 10
harmonics <- fourier(Totalpower_train, K =10)
# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Totalpower_train, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Totalpower_train, K = 10, h = 2*365)
fcast1 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast1
autoplot(fcast1)+
  ylab("Total power retrieved for the last 8 years") + xlab("Year")
# forecasting for order k=5-10 is same with Arima (5,1,2).

##Applying fourier dynamic regreesion model for frequency 365 with order of regressors, k=12 (the number of Fourier sin and cos pairs)
# # Set up harmonic regressors of order 12
harmonics <- fourier(Totalpower_train, K =12)

# Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit2 <- auto.arima(Totalpower_train, xreg = harmonics, seasonal = FALSE)
fit2
# # Forecasts next 2 years
newharmonics <- fourier(Totalpower_train, K = 12, h = 3*365)
fcast2 <- forecast(fit2, xreg = newharmonics)
# # Plot forecasts fcast2
autoplot(fcast2)+
  ylab("Total power retrieved for the last 8 years") + xlab("Year")



##Applying fourier dynamic regreesion model for frequency 365 with order of regressors, k=15 (the number of Fourier sin and cos pairs)
# # Set up harmonic regressors of order 15
harmonics <- fourier(Totalpower_train, K =15)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit3 <- auto.arima(Totalpower_train, xreg = harmonics, seasonal = FALSE)
fit3
# # Forecasts next 2 years
newharmonics <- fourier(Totalpower_train, K = 15, h = 2*365)
fcast3<- forecast(fit3, xreg = newharmonics)
# # Plot forecasts fcast3
autoplot(fcast3)+
  ylab("Total power retrieved for the last 8 years") + xlab("Year")


##Applying fourier dynamic regreesion model for frequency 365 with order of regressors, k=18 (the number of Fourier sin and cos pairs)
# # Set up harmonic regressors of order 18
harmonics <- fourier(Totalpower_train, K =18)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit4 <- auto.arima(Totalpower_train, xreg = harmonics, seasonal = FALSE)
fit4
# # Forecasts next 2 years
newharmonics <- fourier(Totalpower_train, K = 18, h = 2*365)
fcast4<- forecast(fit4, xreg = newharmonics)
# # Plot forecasts fcast4
autoplot(fcast4)+
  ylab("Total power retrieved for the last 8 years") + xlab("Year")


##Applying fourier dynamic regreesion model for frequency 365 with order of regressors, k=20 (the number of Fourier sin and cos pairs)
# # Set up harmonic regressors of order 20
harmonics <- fourier(Totalpower_train, K =20)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit5 <- auto.arima(Totalpower_train, xreg = harmonics, seasonal = FALSE)
fit5
# # Forecasts next 2 years
newharmonics <- fourier(Totalpower_train, K = 20, h = 2*365)
fcast5<- forecast(fit5, xreg = newharmonics)
# # Plot forecasts fcast5
autoplot(fcast5)+
  ylab("Total power retrieved for the last 8 years") + xlab("Year")

accuracy(fcast1,Totalpower_test)
accuracy(fcast2,Totalpower_test)
accuracy(fcast3, Totalpower_test)
accuracy(fcast4,Totalpower_test)
accuracy(fcast5, Totalpower_test)

fcast2%>%autoplot()+autolayer(fitted(fit2), series='Totalpower _train')+autolayer( Totalpower_test)+
  ylab(" Daily Totalpower retrieved") + xlab("Year")
