#loading the daily_means data excel file

library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx')
dim(daily_means)

#Select SUM wind variables by name
library(dplyr)
SUMWind<-daily_means%>% select('date','SUM Wind')

class(SUMWind$date)

#Converting  daily data to monthly data using mean value for using frequency 12 instead of 365 since R programming language not support lag value greater than 350.
library(xts)
SUMWind_month<-apply.monthly(SUMWind, mean)
summary(SUMWind_month$SUM.Wind)

# declare data as time series data
SUMWind_ts<-ts(SUMWind_month$SUM.Wind, start=c(2015,1), end=c(2022,12), frequency=12)

par(mfrow = c(1, 2))
plot(SUMWind_ts, ylab="SUMWind", xlab="year", main='SUM of Wind power production',col = 'cyan4')
plot(SUMWind_Solar_ts, ylab="SUMWind+Solar", xlab="year", main='SUMWind+Solar power production', col='cadetblue3')

#Time series plot for Solar power energy
library(fpp2)
autoplot(SUMWind_ts)+ylab("SUMWind_ts")+xlab("year")+geom_line(colour = "blue",linetype = "solid")+
  theme_minimal()+ ggtitle("SUM of Wind power production (Past 8 years)")+
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))

# From time series plot,we see that multiplicative seasonality is present in the series since the magnitude of seasonal cycles increases with the year, the peaks in the cyclical pattern become higher for the last 2020 and 2022 years compare to other years.

#Series appears use to investigate seasonality
ggseasonplot(SUMWind_ts)+ ggtitle("Seasonal plot: SUM of Wind power production ")+ylab("SUMWind")+xlab("Month")

# perfect seasonality is present in the series.Amount of power production from wind is lower in summer season compare to winter season. 


#Autocorrelation and partial autocorrelation plot

SUMWind_ts%>%ggtsdisplay(lag.max = 48)

#Significant spike  at seasonal and non-seasonal lags indicating non-stationarity in the series with positive and negative correlation.

# checking Number of difference need to make the series stationary
ndiffs(SUMWind_ts)
nsdiffs(SUMWind_ts)

# The data ate clearly non-stationary with seasonality, we will first take seasonal difference to make it stationary.
SUMWind_ts %>% diff(lag=12) %>% ggtsdisplay(xlab="Year", main="Seasonally differenced Solar_ts")

#check for if needed difference for removing trend which gives more statistically significant spike in ACF and PACF.
SUMWind_ts %>% diff(lag=12) %>% diff() %>% ggtsdisplay(lag.max=48, main="Stationary SUM of Wind series")

#The significant spike at lag 12 in the ACF suggests a seasonal component MA(1).
#Also in the PACF, significant spike at lag 12 indicating seasonal AR(1) components.
#In the non-seasonal-lag,significant spikes at lag 1 in ACF indicates nonseasonal MA(1)
#And in significant spikes at lag 1 in PACF indicates nonseasonal MA(1).
#Consequently,with an ARIMA(1,1,1)(1,1,1)12 model, indicating a first  seasonal difference and seasonal MA(1) components


# Create training and test set using 'window' function where using training data to estimate parameters 
# and the test data is used to evaluate its accuracy.

SUMWind_train <- window(SUMWind_ts, start=c(2015,1), end=c(2021,6))
SUMWind_test<-window(SUMWind_ts, start=c(2021,7), end=c(2022,12))
#let's compare several possible models to find out best model based on AIC or RMSE or residuals plot around zero.

SUMWind_fit1<-auto.arima(SUMWind_train, approximation=FALSE, stepwise=FALSE)
summary(SUMWind_fit1)
checkresiduals(SUMWind_fit1)
SUMWind_fit1_forecast<-forecast(SUMWind_fit1,h=24)

SUMWind_fit2<-SUMWind_train %>%Arima(order=c(1,1,0), seasonal = list(order = c(1,1,1), period = 12))
summary(SUMWind_fit2)
SUMWind_fit2%>%residuals() %>% ggtsdisplay()
checkresiduals(SUMWind_fit2)
SUMWind_fit2_forecast<-forecast(SUMWind_fit2,h=24)


SUMWind_fit4<-SUMWind_train%>%Arima(order=c(0,1,1),seasonal=list(order=c(1,1,1), period=12))
summary(SUMWind_fit4)
SUMWind_fit4%>%residuals() %>% ggtsdisplay()
checkresiduals(SUMWind_fit4)
SUMWind_fit4_forecast<-forecast(SUMWind_fit4,h=24)

SUMWind_fit5<-SUMWind_train%>%Arima(order=c(0,1,0),seasonal=list(order=c(0,1,1), period=12))
summary(SUMWind_fit5)
SUMWind_fit5%>%residuals() %>% ggtsdisplay()
checkresiduals(SUMWind_fit5)
SUMWind_fit5_forecast<-forecast(SUMWind_fit5,h=24)

SUMWind_fit6<-SUMWind_train%>%Arima(order=c(0,1,1),seasonal=list(order=c(0,1,1), period=12))
summary(SUMWind_fit6)
SUMWind_fit6%>%residuals() %>% ggtsdisplay()
checkresiduals(SUMWind_fit6)
SUMWind_fit6_forecast<-forecast(SUMWind_fit6,h=24)


SUMWind_fit3<-SUMWind_train %>%Arima(order=c(1,1,1),seasonal =list(order= c(1,1,0), period= 12))
summary(SUMWind_fit3)
SUMWind_fit3%>%residuals() %>% ggtsdisplay()
checkresiduals(SUMWind_fit3)
SUMWind_fit3_forecast<-forecast(SUMWind_fit3,h=24)

model<-list(SUMWind_fit1,SUMWind_fit2,SUMWind_fit3,SUMWind_fit4,SUMWind_fit5,SUMWind_fit6)
model
#Comparing all the model with AIC and AICc, we get SUMWind_fit6 model have the lower value than others. so we can go for forecasting with this model.

#compute the forecast accuracy measures on the test data:
accuracy(SUMWind_fit1_forecast, SUMWind_test)
accuracy(SUMWind_fit2_forecast, SUMWind_test)
accuracy(SUMWind_fit3_forecast, SUMWind_test)
accuracy(SUMWind_fit4_forecast, SUMWind_test)
accuracy(SUMWind_fit5_forecast, SUMWind_test)
accuracy(SUMWind_fit6_forecast, SUMWind_test)
accuracy(SUMWind_fit_forecast, SUMWind_test)
#AICc- lower-case ‘c’ indicates that the value has been calculated from the AIC test corrected for small sample sizes


###forecast with Seasonal Arima model
library(forecast)
Forecast_bestfitted<-forecast(SUMWind_fit6,h =24)
Forecast_bestfitted
autoplot(Forecast_bestfitted,include=72) +
  ylab("SUM of Wind power production") + xlab("Year")+ggtitle("Forecasting SUM of Wind power consumption for next 2 years")

SUMWind_fit6_forecast%>%autoplot(include=72)+autolayer(fitted(SUMWind_fit6), series='SUMWind_train')+autolayer(SUMWind_test)+
  ylab("SUM of power production(monthly)") + xlab("Year")
