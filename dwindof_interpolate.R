library(xts)
#loading the daily_means data file
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx') 


# select column for Windoffshore installed
library(dplyr)
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

#Interpolation of windoffshore_installed yearly for 365 days
df1<- data.frame(YEAR=(c(2015,2016,2017,2018,2019,2020,2021,2022)),
                 Windoffshore_installed=c(993,3283,4131,5051,6393,7504,7774,7787))

year_day <- function(x, years){
  f <- approxfun(years, x)
  n <- length(years)
  days <- unlist(lapply(years[-n], function(y) y + (0:364)/365))
  days <- c(days, years[n])
  list(x = days, y = f(days))
}
data<-as.data.frame(year_day(df1$Windoffshore_installed,df1$YEAR))
head(data,5)
tail(data,5)
dim(data)

# exporting data to excel file to make same dinmension
library(xlsx)
write.xlsx(data,'/Users/latifaredoana/Desktop/Book.xlsx', sheetName = 'sheet2')

#Select first two columns for windoffshore vaiables
daily_means_windoffshore<-daily_means[,c(1,3)]
dim(daily_means_windoffshore)
View(daily_means_windoffshore)
# declare data as time series data
Windoffshore_ts<-ts(daily_means_windoffshore$Windoffshore, start=c(2015,1),end=c(2022,365),frequency=365)


#loading interpolated data with same dimension
data<- read_excel('interpol1.xlsx') 
dim(data)

#Adding interpolated column with existing weekly series
newdata<-cbind(daily_means_windoffshore, interpolate=data$y)
head(newdata)

class(newdata$Windoffshore)
class(newdata$interpolate)

# Devide existing Solar series with inerpolated series
Windoffshore_norm<-newdata$Windoffshore / (newdata$interpolate)
summary(Windoffshore_norm)
# install and loading packages
library(BBmisc)
# method = range for normalisation 
Windoffshore_norm = normalize(Windoffshore_norm, method = "range", range = c(0, 1))

# declare data as time series data
Windoffshore_norm<-ts(Windoffshore_norm, start=c(2015,1),end=c(2022,365),frequency=365)

#Visualization of normalized series
library(fpp2)
library(tseries)
par(mfrow = c(2, 2))
plot(Windoffshore_ts, ylab="daily Windoffshore production", xlab="year", main=' Before normalization',col = 'blue4')
plot(Windoffshore_norm, ylab="daily Windoffshore Production", xlab="year", main='After normalization', col='aquamarine4')
plot(Windonshore_ts, ylab="daily Windonshore production", xlab="year", main=' Before normalization',col = 'blue4')
plot(Windonshore_norm, ylab="daily Windonshore Production", xlab="year", main='After normalization', col='aquamarine4')
adf.test(Windoffshore_ts)
adf.test(Windoffshore_norm)
#It implies that the time series is non-stationary. In simple words, we can say that it possesses some time-dependent structure and does not possess constant variance over time. 
#Time series plot for windoffshore power
library(fpp2)
autoplot(Windoffshore_ts)+ylab("Windoffshore")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Windoffshore production (Past 8 years)") +
  theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))


Windoffshore_train <- window(Windoffshore_norm, start=c(2015,1), end=c(2021,127))
Windoff_test<-window(Windoffshore_norm, start=c(2021,128),end=c(2022,365))

#For daily data we have frequency 365 and we can not model the series arima function since the R software does not support frequency more than 350.The Arima() and auto.arima() functions will allow a seasonal period up to m=350.
#An alternative approach is to use a dynamic harmonic regression model, the number of Fourier terms was selected by minimising the AICc. The order of the ARIMA model is also selected by minimising the AICc, although that is done within the auto.arima() function.

##Applying fourier dynamic regreesion model for frequency 365 with order of regressors, k=6-20 (the number of Fourier sin and cos pairs)
#For all the applied order the ARIMA(1,0,1) model remain same.

# # Set up harmonic regressors of order 6
harmonics <- fourier(Windoffshore_train, K =6)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K = 6, h = 2*365)
fcast1 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast1
autoplot(fcast1)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")



# # Set up harmonic regressors of order 8-15
harmonics <- fourier(Windoffshore_train, K =8)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit2 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit2
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K =8, h = 3*365)
fcast2 <- forecast(fit2, xreg = newharmonics)
# # Plot forecasts fcast2
autoplot(fcast2)+
  ylab("Windoffshoreproduction for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order 15
harmonics <- fourier(Windoffshore_train, K =12)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit3 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit3
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K = 12, h = 3*365)
fcast3 <- forecast(fit3, xreg = newharmonics)
# # Plot forecasts fcast3
autoplot(fcast3)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order 15
harmonics <- fourier(Windoffshore_train, K =15)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit4 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit4
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K = 15, h = 2*365)
fcast4 <- forecast(fit4, xreg = newharmonics)
# # Plot forecasts fcast3
autoplot(fcast4)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")

# # Set up harmonic regressors of order 20
harmonics <- fourier(Windoffshore_train, K =20)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit5 <- auto.arima(Windoffshore_train, xreg = harmonics, seasonal = FALSE)
fit5
# # Forecasts next 2 years
newharmonics <- fourier(Windoffshore_train, K = 20, h = 3*365)
fcast5 <- forecast(fit5, xreg = newharmonics)
# # Plot forecasts fcast4
autoplot(fcast5)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")


accuracy(fcast1, Windoffshore_test)
accuracy(fcast2,Windoffshore_test)
accuracy(fcast3, Windoffshore_test)
accuracy(fcast4, Windoffshore_test)
accuracy(fcast5, Windoffshore_test)

fcast2%>%autoplot()+autolayer(fitted(fit2), series='Windoff _train')+autolayer( Windoff_test)+
  ylab(" Daily Windoffshore production") + xlab("Year")
