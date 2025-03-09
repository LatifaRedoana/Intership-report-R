
#loading the daily_means data excel file

library(forecast)
library(ggplot2)  package for loading excel file
daily_means<- read_excel('daily_means.xlsx')



#Select first two columns for Solar vaiables
daily_means_Solar<-daily_means[,c(1,2)]

dim(daily_means_Solar)
class(daily_means_Solar$date)


#Converting daily data  to monthly data using mean value for using frequency 12 instead of 365 since R programming language not support lag value greater than 350.
library(xts)
daily_Solar_month<-apply.monthly(daily_means_Solar, mean)

summary(daily_Solar_month$Solar)

# declare data as time series data
Solar_ts<-ts(daily_Solar_month$Solar, start=c(2015,1), end=c(2022,12), frequency=12)
plot(decompose(Solar_ts))
#Can be done using this also
Solar_ts1<-ts(daily_means_Solar$Solar, start=c(2015,1), end=c(2022,12), frequency=365, as.POSIXct(daily_means_Solar$date, format="%Y-%m-%d"))

# Create training and test set using 'window' function where using training data to estimate parameters 
# and the test data is used to evaluate its accuracy.

solar_train <- window(Solar_ts, start=c(2015,1), end=c(2021,6))
Solar_test<-window(Solar_ts, start=c(2021,7), end=c(2022,12))

#Time series plot for Solar power energy
library(fpp2)
autoplot(Solar_ts)+ylab("Solar")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Time plot: Monthly Solar energy production") +theme(plot.title = element_text(hjust=0.5, size=16, face='bold'))

#Series appears use to investigate seasonality
ggseasonplot(Solar_ts)+ ggtitle("Seasonal plot: Monthly Solar energy production")+theme(plot.title = element_text(hjust=0.5, size=16,face='bold'))

#power consumption from Solar is higher in summer season compare to winter season.

#Autocorrelation and partial autocorrelation plot
#Significant spike indicating non-stationarity for seasonality.
#since the magnitude of seasonal cycles increases with trend,its implies multiplicative seasonality 
#the peaks in the cyclical pattern become higher.
par(mfcol = c(1,2))
Solar_ts%>%ggtsdisplay(xlab="Year", main="ACF and PACF before taking difference")

Solar_ts %>% diff(lag=12) %>% diff() %>% ggtsdisplay(xlab="Year", main='ACF and PACF plot after taking difference')
# The data ate clearly non-stationary with seasonality, we will take seasonal difference to make it stationary.
Solar_ts %>% diff(lag=12) %>% ggtsdisplay(xlab="Year", main="Seasonally differenced Solar_ts")

#check for if needed difference for removing trend which gives more statistically significant spike in ACF and PACF.
Solar_ts %>% diff(lag=12) %>% diff() %>% ggtsdisplay()

#The significant spike at lag 12 in the ACF suggests a seasonal component MA(1).
#Also in the PACF, significant spike at lag 12 indicating seasonal AR(1) components.
#In the non-seasonal-lag, no significant spikes.Consequently,with an ARIMA(1,1,1)(1,1,1)12 model, indicating a first  seasonal difference and seasonal MA(1) components

#let's compare several possible models to find out best model based on AIC or RMSE or residuals plot around zero.
fit1<-solar_train %>%Arima(order=c(1,1,1), seasonal = list(order = c(1,1,1), period = 12))
summary(fit1)
checkresiduals(fit1)
fit1_forecast<-forecast(fit1,h=3*12)

fit2<-solar_train%>%Arima(order=c(1,0,0),seasonal=list(order=c(1,1,0), period=12))
summary(fit2)
checkresiduals(fit2)
fit2_forecast<-forecast(fit2,h=3*12)


fit3<-solar_train%>%Arima(order=c(0,0,1),seasonal=list(order=c(1,1,0), period=12))
summary(fit3)
checkresiduals(fit3)
fit3_forecast<-forecast(fit3,h=3*12)         


fit4<-solar_train%>%Arima(order=c(0,0,0),seasonal=list(order=c(1,1,1), period=12))
summary(fit4)
checkresiduals(fit4)
fit4_forecast<-forecast(fit4,h=3*12)

fit5<-auto.arima(solar_train, approximation=FALSE, stepwise=FALSE)
summary(fit5)
checkresiduals(fit5)      
fit5_forecast<-forecast(fit5,h=3*12)


#compute the forecast accuracy measures on the test data:
accuracy(fit1_forecast, Solar_test)
accuracy(fit2_forecast, Solar_test)
accuracy(fit3_forecast, Solar_test)
accuracy(fit4_forecast, Solar_test)
accuracy(fit5_forecast, Solar_test)

#Comparing all the model  with RMSE, we get fit1 model have the lower value than others. so we can go for forecasting with this model.

###forecast with Seasonal Arima model


fit1_forecast%>%autoplot(include=72)+autolayer(fitted(fit1), series='Solar_train')+autolayer(Solar_test)+
  ylab("Monthly Solar energy production") + xlab("Year")




