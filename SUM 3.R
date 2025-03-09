#loading the daily_means data excel file
library('ggplot2')
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx')


#Select SUM wind variables by name
library(dplyr)
SUMWind_Solar<-daily_means%>% select('date', 'SUM Wind + Solar')
class(SUMWind_Solar$date)

#Converting daily data to monthly data using mean value using frequency 12 instead of 365 since R programming language not support lag value greater than 350.
library(xts)
SUMWind_Solar_month<-apply.monthly(SUMWind_Solar, mean)
dim(SUMWind_Solar_month)
summary(SUMWind_Solar_month$SUM.Wind...Solar)
#the minimum and maximum value are higher than SUM Wind variable and so all others measures of central tendency(mean,median, mode) are higher equally.so we can say power consumption from including solar with be increased.

# declare data as time series data
SUMWind_Solar_ts<-ts(SUMWind_Solar_month$SUM.Wind...Solar, start=c(2015,1), end=c(2022,12), frequency=12)

#Time series plot for Solar power energy
library(fpp2)

autoplot(SUMWind_Solar_ts)+ylab("SUMWind_Solar_ts")+xlab("year")+geom_line(colour = "blue",linetype = "solid")+
  theme_minimal()+ ggtitle("SUM of all power production (Past 8 years)")+
  theme(plot.title = element_text(hjust=0.5, size=15, face="bold"))


# From time series plot,we see that multiplicative seasonality is present in the series since the magnitude of seasonal cycles increases with the year, the peaks in the cyclical pattern become higher for the last 2020 and 2022 years compare to other years.

#Series appears use to investigate seasonality
ggseasonplot(SUMWind_Solar_ts)+ ggtitle("Seasonal plot: SUM Wind+Solar power production ")+ylab("SUMWind+Solar")+xlab("Month")
# we can say, combining all source , on average all years have approximately equal amount of power consumption although its higher in the beginning of 2020 and 2022 year.Not so much bigger comparable magnitude. 


#Autocorrelation and partial autocorrelation plot

SUMWind_Solar_ts%>%ggtsdisplay(lag.max = 48)

#Significant spike  at seasonal and non-seasonal lags indicating non-stationarity in the series.

# The data ate clearly non-stationary with trend, we will take first difference to make it stationary.
SUMWind_Solar_ts %>% diff(lag=12) %>% ggtsdisplay(xlab="Year", main="Seasonally differenced Solar_ts")

#check for if needed seasonal difference for removing seasonality. 
SUMWind_Solar_ts %>% diff(lag=12)  %>% diff  %>% ggtsdisplay(lag.max=48, main="Stationary SUM of Wind+Solar series")

#The significant spike at lag 1 in the ACF suggests a non-seasonal component MA(1).
#Also in the PACF, significant spike at lag 1 indicating non-seasonal AR(1) components.
#In the seasonal lag,significant spikes at lag 12 in ACF indicates seasonal MA(1)
#Consequently,with an ARIMA(1,1,1)(0,1,1)12 model, indicating a first  seasonal difference and seasonal MA(1) components

SUMWind_Solar_train <- window(SUMWind_ts, start=c(2015,1), end=c(2021,6))
SUMWind_Solar_test<-window(SUMWind_ts, start=c(2021,7), end=c(2022,12))
#let's compare several possible models to find out best model based on AIC or RMSE or residuals plot around zero.


SUMWind_Solar_fit1<-auto.arima(SUMWind_Solar_train, approximation=FALSE, stepwise=FALSE)
summary(SUMWind_Solar_fit1)
checkresiduals(SUMWind_Solar_fit1)
SUMWind_Solar_fit1_forecast<-forecast(SUMWind_Solar_fit1,h=24)

SUMWind_Solar_fit1<-SUMWind_Solar_train %>%Arima(order=c(1,1,0), seasonal = list(order = c(0,1,1), period = 12))
summary(SUMWind_Solar_fit1)
checkresiduals(SUMWind_Solar_fit1)
SUMWind_Solar_fit1_forecast<-forecast(SUMWind_Solar_fit1,h=24)

SUMWind_Solar_fit2<-SUMWind_Solar_train %>%Arima(order=c(1,1,2), seasonal = list(order = c(1,1,0), period = 12))
summary(SUMWind_Solar_fit2)
SUMWind_Solar_fit1%>%residuals() %>% ggtsdisplay()
checkresiduals(SUMWind_Solar_fit2)
SUMWind_Solar_fit2_forecast<-forecast(SUMWind_Solar_fit2,h=36)

SUMWind_Solar_fit3<-SUMWind_Solar_train %>%Arima(order=c(2,1,1),seasonal=list(order=c(0,1,1), period=12))
summary(SUMWind_Solar_fit3)
checkresiduals(SUMWind_Solar_fit3)
SUMWind_Solar_fit3_forecast<-forecast(SUMWind_Solar_fit3,h=24)

SUMWind_Solar_fit4<-SUMWind_Solar_train %>%Arima(order=c(1,1,1),seasonal=list(order=c(0,1,1), period=12))
summary(SUMWind_Solar_fit4)
SUMWind_Solar_fit4%>%residuals() %>% ggtsdisplay()
checkresiduals(SUMWind_Solar_fit4)
SUMWind_Solar_fit4_forecast<-forecast(SUMWind_Solar_fit4,h=24)

SUMWind_Solar_fit5<-SUMWind_Solar_train %>%Arima(order=c(1,1,1),seasonal=list(order=c(1,1,0), period=12))
summary(SUMWind_Solar_fit5)
SUMWind_Solar_fit5%>%residuals() %>% ggtsdisplay()
checkresiduals(SUMWind_Solar_fit5)
SUMWind_Solar_fit5_forecast<-forecast(SUMWind_Solar_fit5,h=24)

#Comparing all the model with AIC and AICc, we get SUMWind_Solar_ts_fit4 model have the lower value than others. so we can go for forecasting with this model.
#compute the forecast accuracy measures on the test data:
accuracy(SUMWind_Solar_fit1_forecast, SUMWind_Solar_test)
accuracy(SUMWind_Solar_fit2_forecast, SUMWind_Solar_test)
accuracy(SUMWind_Solar_fit3_forecast, SUMWind_Solar_test)
accuracy(SUMWind_Solar_fit4_forecast, SUMWind_Solar_test)
accuracy(SUMWind_Solar_fit5_forecast,SUMWind_Solar_test)
#AICc- lower-case ‘c’ indicates that the value has been calculated from the AIC test corrected for small sample sizes


###forecast with Seasonal Arima model
library(forecast)
Forecast_bestfitted<-forecast(SUMWind_Solar_ts_fit4,h =24)
Forecast_bestfitted
autoplot(Forecast_bestfitted,include=72) +
  ylab("SUM Wind+Solar power production") + xlab("Year")+ggtitle('forecast forSUM Wind+Solar power production ')


SUMWind_Solar_fit2_forecast%>%autoplot(include=72)+autolayer(fitted(SUMWind_Solar_fit2), series='SUMWind_Solar_train')+autolayer(SUMWind_Solar_test)+
  ylab("SUM Wind+Solar power production(monthly)") + xlab("Year")
