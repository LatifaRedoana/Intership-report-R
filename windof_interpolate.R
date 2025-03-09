library(xts)
library(dplyr)
#loading the daily_means data file
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx') 

# select column for Windoffshore installed
Windoffshore_installed<- daily_means%>%select(date,`Wind offshore installed`)
head(Windoffshore_installed,5)

#create data frame for yearly data points
year<-c(2015,2016,2017,2018,2019,2020,2021,2022)
Windoffshore_product=c(993,3283,4131,5051,6393,7504,7774,7787)
Windoffshore_installed<-data.frame(year,Windoffshore_product)
Windoffshore_installed

#plot the Windoffshore_installed series
plot(Windoffshore_installed, type='o',main='Windoffshore_installed yearly')
# Windoffshore installed  by year

#Interpolation of windoffshore_installed yearly for 52 weeks
df1<- data.frame(YEAR=(c(2015,2016,2017,2018,2019,2020,2021,2022)),
                 Windoffshore_installed=c(993,3283,4131,5051,6393,7504,7774,7787))

year_week <- function(x, years){
  f <- approxfun(years, x)
  n <- length(years)
  weeks <- unlist(lapply(years[-n], function(y) y + (0:51)/52))
  weeks <- c(weeks, years[n])
  list(x = weeks, y = f(weeks))
}
data<-as.data.frame(year_week(df1$Windoffshore_installed,df1$YEAR))



#Select first two columns for windoffshore vaiables
daily_means_windoffshore<-daily_means[,c(1,3)]

#Converting  daily data to weekly using mean value for using frequency 52. 
daily_windoffshore_week<-apply.weekly(daily_means_windoffshore, mean)
dim(daily_windoffshore_week)

#loading interpolated data with same dimension
data<- read_excel('data1.xlsx') 
dim(data)



#Adding interpolated column with existing weekly series
newdata<-cbind(daily_windoffshore_week, interpolate=data$y)


# Devide existing Solar series with inerpolated series
devide_Windoffshore<-newdata$Wind.offshore / (newdata$interpolate)



# install and loading packages
library(BBmisc)
# method = range for normalisation 
Windoffshore_norm = normalize(devide_Windoffshore, method = "range", range = c(0, 1))

# declare data as time series data
Windoffshore_norm<-ts(Windoffshore_norm, start=c(2015,1),end=c(2022,54),frequency=52)
Windoffshore_ts<-ts(daily_windoffshore_week$Wind.offshore, start=c(2015,1),end=c(2022,54),frequency=52)

#Time series plot for windoffshore power
par(mfrow = c(2, 2))
plot(Windoffshore_ts, ylab="Weekly Windoffshore production", xlab="year", main='Before normalization',col = 'black')
plot(Windoffshore_norm, ylab="Weekly Windoffshore production", xlab="year", main='After normalization', col='blue4')
plot(Windonshore_ts, ylab="Weekly Windonshore production", xlab="year", main=' Before normalization')
plot(Windonshore_norm, ylab="Weekly Windonshore production", xlab="year", main=' After normalization', col='blue4')

library(fpp2)
autoplot(Windoffshore)+ylab("Windoffshore_norm")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Windoffshore production (Past 8 years)") +
  theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))

#Series appears use to investigate seasonality
ggseasonplot(Windoffshore_norm)+ ggtitle("Seasonal plot:Windoffshore production energy")
adf.test(Windoffshore_norm)
# Create training and test set using 'window' function where using training data to estimate parameters 
# and the test data is used to evaluate its accuracy.
Windoffshore_train <- window(Windoffshore_norm, start=c(2015,1), end=c(2021,26))
Windoffshore_test<-window(Windoffshore_norm, start=c(2021,27), end=c(2022,52))

# # Set up harmonic regressors of order 6
harmonics <- fourier(Windoffshore_train, K =6)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K = 6, h = 2*52)
fcast1 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast1
autoplot(fcast1)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order 8
harmonics <- fourier(Windoffshore_train, K =8)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit2 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit2
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K =8, h = 2*52)
fcast2 <- forecast(fit2, xreg = newharmonics)
# # Plot forecasts fcast2
autoplot(fcast2)+
  ylab("Windoffshoreproduction for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order untill 10
harmonics <- fourier(Windoffshore_train, K =10)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit3 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit3
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K = 10, h = 3*52)
fcast3 <- forecast(fit3, xreg = newharmonics)
# # Plot forecasts fcast3
autoplot(fcast3)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order 11-15
harmonics <- fourier(Windoffshore_train, K =15)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit4 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit4
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K = 15, h = 2*52)
fcast4 <- forecast(fit4, xreg = newharmonics)
# # Plot forecasts fcast4
autoplot(fcast4)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")
  
# # Set up harmonic regressors of order 17
harmonics <- fourier(Windoffshore_train, K =17)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit5 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit5
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K = 17, h = 3*52)
fcast5 <- forecast(fit5, xreg = newharmonics)
# # Plot forecasts fcast5
autoplot(fcast5)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")



accuracy(fcast1, Windoffshore_test)
accuracy(fcast2, Windoffshore_test)
accuracy(fcast3, Windoffshore_test)
accuracy(fcast4, Windoffshore_test)
accuracy(fcast5, Windoffshore_test)

fcast3%>%autoplot()+autolayer(fitted(fit3), series=' Windoffshore_train')+autolayer( Windoffshore_test)+
  ylab(" Weekly Windoffshore production") + xlab("Year")