#loading the daily_means data file
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx')

# preliminary analysis for Windoffshore variable
daily_means_windoff<-daily_means[,c(1,3)]
dim(daily_windoff_month)
View(daily_means)

#Converting  daily data to monthly data using mean value for using frequency 12 instead of 365 since R programming language not support lag value greater than 350.
library(xts)
daily_windoff_month<-apply.monthly(daily_means_windoff, mean)
data<-cbind(daily_windoff_month,daily_windon_month)
View(data)

summary(daily_windoff_month$Windoffshore)

#Declare data as time series data
Windoff_ts<-ts(daily_windoff_month$Wind.offshore, start=c(2015,1), end=c(2022,12), frequency=12)


# Create training and test set using 'window' function where using training data to estimate parameters 
# and the test data is used to evaluate its accuracy.

Windoff_train <- window(Windoff_ts, start=c(2015,1), end=c(2021,6))
Windoff_test<-window(Windoff_ts, start=c(2021,7), end=c(2022,12))

#Time series plot for Windoffshore_train power energy
library(fpp2)

par(mfrow = c(1, 2))

plot(Windoff_ts, ylab="Windoffshore", xlab="year", main='Windoffshore power production',col = 'cyan3')
plot(Windon_ts, ylab="Windonshore", xlab="year", main='Windonshore power production', col='aquamarine4')

autoplot(Windoff_ts)+ylab("Windoffshore_trainset")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Windoffshore power consumption (Past 7 years)") +
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))

#We see from time series plot, multiplicative seasonality is present in the series.
#since the magnitude of seasonal cycles increases with trend increase,
#the peaks in the cyclical pattern become higher and troughs become deeper.

par(mfrow = c(1, 2))
#Series appears use to investigate seasonality
ggseasonplot(Windoff_ts)+ ggtitle("Seasonal plot: Monthly Windoffshore production")+theme(plot.title = element_text(hjust=0.5, size=16,face='bold'))
#Windoffshore power production is higer in the winter season compare to summer season
#indicating seasonality is present in the data.


#Autocorrelation and partial autocorrelation plot
Windoff_ts%>%ggtsdisplay(lag.max = 48,main='')
Acf(Windoff_ts,main='ggh')
#Significant spike indicating non-stationarity for seasonality and trend present in the data.
#the auto-correlations is larger for the seasonal lags(12,24,..) than other lags indicates seasonality.
#we need to take difference to make the series stationary.

# checking Number of difference need to make the series stationary
ndiffs(Windoff_ts)
nsdiffs(Windoff_ts)

# The data ate clearly non-stationary with seasonality, we will take seasonal difference to make it stationary.
Windoff_ts %>% diff(lag=12) %>% ggtsdisplay(xlab="Year", main="Seasonally differenced windoff_ts")

#check for if needed difference for trend which gives more statistically significant spike in ACF and PACF.
Windoff_ts %>% diff(lag=12) %>% diff() %>% ggtsdisplay(xlab="Year", main="Stationary Windoffshore series")

#the significant spike at lag 1 in the ACF suggests a non-seasonal component MA(1).
#Also in the PACF, significant spike at lag 1 indicating non-seasonal AR(1) components. 
#Also in the PACF, significant spike at lag 12 indicating seasonal AR(1) components
#Consequently,with an ARIMA(1,1,1)(1,1,0)12 model could be one selected fitted model.

#let's compare several possible models to find out best model based on AIC or RMSE or residuals plot around zero.


fit1<-Windoff_train%>%Arima(order=c(0,1,1),seasonal=c(1,1,0))
summary(fit1)
checkresiduals(fit1)
fit1_forecast<-forecast(fit1,h=24)

fit2<-Windoff_train%>%Arima(order=c(1,1,0),seasonal=c(1,1,1))
summary(fit2)
checkresiduals(fit2)
fit2_forecast<-forecast(fit2,h=24)

fit3<-Windoff_train%>%Arima(order=c(1,1,2),seasonal=c(1,1,0))
summary(fit3)
checkresiduals(fit3)
fit3_forecast<-forecast(fit3,h=24)

fit4<-Windoff_train%>%Arima(order=c(1,1,3),seasonal=c(0,1,1))
summary(fit4)
checkresiduals(fit4)
fit4_forecast<-forecast(fit4,h=36)

fit5<-auto.arima(Windoff_train, approximation=FALSE, stepwise=FALSE)
summary(fit5)
checkresiduals(fit5)
fit5_forecast<-forecast(fit5,h=24)
 

#compute the forecast accuracy measures on the test data:
accuracy(fit1_forecast, Windoff_test)
accuracy(fit2_forecast, Windoff_test)
accuracy(fit3_forecast, Windoff_test)
accuracy(fit4_forecast, Windoff_test)
accuracy(fit5_forecast, Windoff_test)

#Comparing all the model  with RMSE, we get fit5 model have the lower value than others. so we can go for forecasting with this model.

fit4_forecast%>%autoplot(include=72) +
  ylab("Windoffshore power production") + xlab("Year")


fit4_forecast%>%autoplot(include=72)+autolayer(fitted(fit4), series='Windoff_train')+autolayer(Windoff_test)+
  ylab("Monthly Windoffshore power production") + xlab("Year")
