#loading the daily_means data excel file
library(dplyr)

library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx')
View(daily_means)
dim(daily_means)

#Select two columns for Power retrieved total vaiables
library(dplyr)
daily_Totalpower<-daily_means%>%select(date,`Power.retrieved.total`)
head(daily_Totalpower)
dim(daily_Totalpower)
class(daily_Totalpower$date)


#Converting daily to monthly data using mean value for using frequency 12.
library(xts)
monthly_Totalpower<-apply.monthly(daily_Totalpower, mean)
head(monthly_Totalpower)
summary(monthly_Totalpower$Power.retrieved.total)


# declare data as time series data
Totalpower_ts<-ts(monthly_Totalpower$Power.retrieved.total, start=c(2015,1,1), frequency=12)


#Time series plot for Power retrieved total energy
library(fpp2)
autoplot(Totalpower_ts)+ylab("Totalpower")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle(" Time plot: Monthly Total power retrieved ") +
  theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))


#Series appears use to investigate seasonality
ggseasonplot(Totalpower_ts)+ ggtitle("Seasonal plot: Monthly Total power retrieved")+theme(plot.title = element_text(hjust=0.5, size=20, face="bold"))
#Total power retrieved is higher in winter season compare to summer season.

acf(Totalpower_ts, lag.max =24 )
pacf(Totalpower_ts, lag.max = 24)
Totalpower_ts%>%ggtsdisplay(xlab="Year", main='Non-Stationary Total power retrieved series')


# checking Number of difference need to make the series stationary
ndiffs(Totalpower_ts)
nsdiffs(Totalpower_ts)


# The data ate clearly non-stationary with seasonality, we will take seasonal difference to make it stationary.
Totalpower_ts %>% diff(lag=12) %>% ggtsdisplay(xlab="Year", main="Seasonally differenced Solar_ts")


#check for if needed difference for removing trend which gives more statistically significant spike in ACF and PACF.
Totalpower_ts %>% diff(lag=12) %>% diff() %>% ggtsdisplay(xlab="Year", main='Stationary Total power retrieved series')


#The significant spike at lag 12 in the ACF suggests a seasonal component MA(1).
#Also in the PACF, significant spike at lag 12 indicating seasonal AR(1) components.
#In the non-seasonal-lag, there are no significant spikes.Consequently,with an ARIMA(0,0,0)(1,1,1)12 model, indicating a first  seasonal difference and seasonal MA(1) components
# Create training and test set using 'window' function where using training data to estimate parameters 
# and the test data is used to evaluate its accuracy.

Totalpower_train <- window(Totalpower_ts, start=c(2015,1), end=c(2021,6))
Totalpower_test<-window(Totalpower_ts, start=c(2021,7), end=c(2022,12))
#let's compare several possible models to find out best model based on AIC or RMSE or residuals plot around zero.

fit1<-Totalpower_train %>%Arima(order=c(0,1,0), seasonal = list(order = c(1,1,1), period = 12))
summary(fit1)
fit1%>%residuals() %>% ggtsdisplay()
checkresiduals(fit1)
fit1_forecast<-forecast(fit1,h=24)



fit2<-Totalpower_train %>%Arima(order=c(1,1,0), seasonal = list(order = c(1,1,1), period = 12))
summary(fit2)
checkresiduals(fit1)
fit2%>%residuals() %>% ggtsdisplay()
fit2_forecast<-forecast(fit2,h=24)


fit3<-Totalpower_train %>%Arima(order=c(1,1,0), seasonal = list(order = c(2,1,1), period = 12))
summary(fit3)
checkresiduals(fit1)
fit3%>%residuals() %>% ggtsdisplay()
fit3_forecast<-forecast(fit3,h=24)


fit4<-Totalpower_train %>%Arima(order=c(1,1,0), seasonal = list(order = c(2,1,0), period = 12))
summary(fit4)
checkresiduals(fit1)
fit4%>%residuals() %>% ggtsdisplay()
fit4_forecast<-forecast(fit4,h=24)

fit5<-Totalpower_train %>%Arima(order=c(2,0,0), seasonal = list(order = c(2,1,0), period = 12))
fit%>%residuals() %>% ggtsdisplay()
checkresiduals(fit5)
fit5_forecast<-forecast(fit5,h=3*12)

fit5<-auto.arima(Totalpower_train, approximation=FALSE, seasonal=TRUE, stepwise=FALSE)
summary(fit5)
fit5%>%residuals() %>% ggtsdisplay()
fit5_forecast<-forecast(fit5,h=24)


#compute the forecast accuracy measures on the test data:
accuracy(fit1_forecast, Totalpower_test)
accuracy(fit2_forecast, Totalpower_test)
accuracy(fit3_forecast, Totalpower_test)
accuracy(fit4_forecast, Totalpower_test)
accuracy(fit5_forecast, Totalpower_test)

###forecast with best Seasonal Arima model
library(forecast)
fcast<-forecast(fit4,h=24)
fcast%>%autoplot(include=72) +
  ylab("Total power retrieved for the last 8 years") + xlab("Year")

fit5_forecast%>%autoplot(include=72)+autolayer(fitted(fit5), series='Totalpower_train')+autolayer(Totalpower_test)+
  ylab(" Total power Retrieved") + xlab("Year")
