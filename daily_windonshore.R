
#loading the daily_means data file
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx')

#Select Windonshore columns for analysis
daily_means_windon<-daily_means[,c(1,4)]
dim(daily_means_windon)

#Converting monthly data to daily data using mean value for using frequency 12 instead of 365 since R programming language not support lag value greater than 350.
library(xts)
daily_windon_month<-apply.monthly(daily_means_windon, mean)
dim(daily_windon_month)
summary(daily_windon_month$Windonshore)

# declare data as time series data
Windon_ts<-ts(daily_windon_month$Windonshore, start=c(2015,1), end=c(2022,12), frequency=12)


#Time series plot for Windonshore power energy
library(fpp2)
par(mfrow = c(1, 2))
autoplot(Windon_ts)+ylab("Windonshore")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Windonshore power production (Past 8 years)") +
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))

#We see from time series plot, multiplicative seasonality is present in the series since the magnitude of seasonal cycles increases with the year, the peaks in the cyclical pattern become higher and troughs become deeper.

#Series appears use to investigate seasonality
ggseasonplot(Windon_ts)+ ggtitle("Seasonal plot: Monthly Windonshore production")+theme(plot.title = element_text(hjust=0.5, size=16,face='bold'))

#Autocorrelation and partial autocorrelation plot

Windon_ts%>%ggtsdisplay()

#Significant spike indicating non-stationarity for seasonality and trend.
#the autocorrelations is larger for the seasonal lags(12,24,..) than for other lags.
#both seasonal and non-seasonal significant spike are present. 
#so we need to take both difference to make the series stationary.

# checking Number of difference need to make the series stationary
ndiffs(Windon_ts)
nsdiffs(Windon_ts)

# The data ate clearly non-stationary with seasonality, we will take seasonal difference to make it stationary.
Windon_ts %>% diff(lag=12) %>% ggtsdisplay(xlab="Year", main="Seasonally differenced windoff_ts")

#check for if needed difference for trend which gives more statistically significant spike in ACF and PACF.
Windon_ts %>% diff(lag=12) %>% diff() %>% ggtsdisplay(xlab="Year", main="Stationary Windonshore series")

#comment: the significant spike at lag 1 in the ACF suggests a non-seasonal componentMA(1)
#Also in the PACF, significant spike at lag 1 indicating non-seasonal AR(1) components. 
#In the seasonal-lag 12, 24,.. no significant spikes.Consequently,with an ARIMA(1,1,1)(1,1,1)12 model, indicating a first  seasonal difference and seasonal MA(1) components

# Create training and test set using 'window' function where using training data to estimate parameters 
# and the test data is used to evaluate its accuracy.

Windon_train <- window(Windon_ts, start=c(2015,1), end=c(2021,6))
Windon_test<-window(Windon_ts, start=c(2021,7), end=c(2022,12))

#let's compare several possible models to find out best model based on AIC or RMSE or residuals plot around zero.
fit1<-Windon_train %>%Arima(order=c(1,1,1), seasonal=c(1,1,1)) 
summary(fit1)
fit1%>%residuals() %>% ggtsdisplay()
checkresiduals(fit1)
fit1_forecast<-forecast(fit1,h=24)

fit2<-Windon_train%>%Arima(order=c(2,1,1),seasonal=c(1,1,1))
summary(fit2)
fit2%>%residuals() %>% ggtsdisplay()
checkresiduals(fit2)
fit2_forecast<-forecast(fit2,h=24)

fit3<-Windon_train%>%Arima(order=c(2,1,2),seasonal=c(1,1,1))
summary(fit3)
fit3%>%residuals() %>% ggtsdisplay()
checkresiduals(fit3)
fit3_forecast<-forecast(fit3,h=24)


fit4<-Windon_train%>%Arima(order=c(0,1,1),seasonal=c(1,1,0))
summary(fit4)
fit4%>%residuals() %>% ggtsdisplay()
checkresiduals(fit4)
fit4_forecast<-forecast(fit4,h=36)

fit5<-Windon_train%>%Arima(order=c(1,1,0),seasonal=c(0,1,1))
summary(fit5)
fit5%>%residuals() %>% ggtsdisplay()
checkresiduals(fit5)
fit5_forecast<-forecast(fit5,h=24)

fit6<-auto.arima(Windon_train, approximation=FALSE, stepwise=FALSE)
summary(fit6)
fit6%>%residuals() %>% ggtsdisplay()
checkresiduals(fit6)
fit6_forecast<-forecast(fit6,h=24)

#compute the forecast accuracy measures on the test data:
accuracy(fit1_forecast, Windon_test)
accuracy(fit2_forecast, Windon_test)
accuracy(fit3_forecast, Windon_test)
accuracy(fit4_forecast, Windon_test)
accuracy(fit5_forecast, Windon_test)
accuracy(fit6_forecast, Windon_test)
#Comparing all the model  with RMSE keeping same difference, we get fit4 model have the lower value than others. so we can go for forecasting with this model.

###forecast with Seasonal Arima model
library(forecast)

forecast(fit4,h=24)%>%autoplot(include=72)+
  ylab("Windonshore power production for the last 8 years") + xlab("Year")


fit4_forecast%>%autoplot(include=72)+autolayer(fitted(fit4), series='Windon_train')+autolayer(Windon_test)+
  ylab("Monthly Windonshore power production") + xlab("Year")
