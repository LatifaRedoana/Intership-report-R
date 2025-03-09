#loading the daily_means data excel file

library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx')
dim(daily_means)

#Select two columns for Power retrieved total vaiables
library(dplyr)
daily_Totalpower<-daily_means%>%select(date,`Power retrieved total`)
head(daily_Totalpower)
dim(daily_Totalpower)
class(daily_Totalpower$date)


#Converting daily to monthly data using mean value for using frequency 12.
library(xts)
weekly_Totalpower<-apply.weekly(daily_Totalpower, mean)
head(weekly_Totalpower)
summary(weekly_Totalpower$Power.retrieved.total)


# declare data as time series data
weekly_Totalpower_ts<-ts(weekly_Totalpower$Power.retrieved.total, start=c(2015,1,1), frequency=52)

#Time series plot for Power retrieved total energy
library(fpp2)
autoplot(weekly_Totalpower_ts)+ylab("Totalpower")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle(" Weekly Total power retrieved") +
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))
par(mfrow = c(1, 2))
plot(weekly_Totalpower_ts, ylab="Windonshore", xlab="year", main='Weekly Production before normalization')

#Series appears use to investigate seasonality
ggseasonplot(weekly_Totalpower_ts)+ ggtitle("Seasonal plot: Weekly Total power retrieved")+ylab("Totalpower")+xlab("year")+theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))

#Total power retrieved is higher in winter season compare to summer season.

acf(weekly_Totalpower_ts, lag.max =104 )
pacf(weekly_Totalpower_ts, lag.max = 104)
#Significant spike both seasonal and non-seasonal significant spike are present indicating non-stationarity for seasonality and trend.

adf.test(weekly_Totalpower_ts)


#The approach we can use a dynamic harmonic regression model, where the number of Fourier terms was selected by minimising the AICc. The order of the ARIMA model is also selected by minimising the AICc, although that is done within the auto.arima() function. 
#Dynamic harmonic regression is based on the principal that a combination of sine and cosine functions approximate the periodic function to model the seasonality.
#The optimal model has the lowest AICc, so start with K=1 and increase until the AICc is no longer decreasing. K cannot be greater than m/2, where m is the seasonal period.

Totalpower_train <- window(weekly_Totalpower_ts, start=c(2015,1), end=c(2021,26))
Totalpower_test<-window(weekly_Totalpower_ts, start=c(2021,27), end=c(2022,52))
# # Set up harmonic regressors of order 3
harmonics <- fourier(Totalpower_train, K =3)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Totalpower_train, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Totalpower_train, K = 3, h = 2*52)
fcast1 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast1
autoplot(fcast1)+
  ylab("Total power retrieved for the last 8 years") + xlab("Year")
#Forecast with Model ARIMA(3,1,3) is for harmonic regressors of order K=3.




# # Set up harmonic regressors of order 6
harmonics <- fourier(Totalpower_train, K =6)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit2 <- auto.arima(Totalpower_train, xreg = harmonics, seasonal = FALSE)
fit2
# # Forecasts next 2 years
newharmonics <- fourier(Totalpower_train, K = 6, h = 3*52)
fcast2 <- forecast(fit2, xreg = newharmonics)
# # Plot forecasts fcast2
autoplot(fcast2)+
  ylab("Total power retrieved for the last 8 years") + xlab("Year")
#Forecast with Model ARIMA(5,1,4) is for harmonic regressors of order K=6.



# # Set up harmonic regressors of order 7
harmonics <- fourier(Totalpower_train, K =7)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit3 <- auto.arima(Totalpower_train, xreg = harmonics, seasonal = FALSE)
fit3
# # Forecasts next 2 years
newharmonics <- fourier(Totalpower_train, K = 7, h = 2*52)
fcast3 <- forecast(fit3, xreg = newharmonics)
# # Plot forecasts fcast3
autoplot(fcast3)+
  ylab("Total power retrieved for the last 8 years") + xlab("Year")
#Forecast with Model ARIMA(3,1,3) is for harmonic regressors of order K=7.



# # Set up harmonic regressors of order 8-20
harmonics <- fourier(Totalpower_train, K =9)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit4 <- auto.arima(Totalpower_train, xreg = harmonics, seasonal = FALSE)
fit4
# # Forecasts next 2 years
newharmonics <- fourier(Totalpower_train, K = 9, h = 3*52)
fcast4 <- forecast(fit4, xreg = newharmonics)
# # Plot forecasts fcast4
autoplot(fcast2)+
  ylab("Total power retrieved for the last 8 years") + xlab("Year")
#Forecast with Model ARIMA(2,1,1) is remain same both for harmonic regressors of order K=8-20.

accuracy(fcast1, Totalpower_test)
accuracy(fcast2,Totalpower_test)
accuracy(fcast3, Totalpower_test)
accuracy(fcast4,Totalpower_test)
accuracy(fcast5, Totalpower_test)
fcast2%>%autoplot()+autolayer(fitted(fit2), series=' Totalpower_train')+autolayer( Totalpower_test)+
  ylab(" Weekly Totalpower production") + xlab("Year")
