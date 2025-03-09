library(xts)
#loading the daily_means data file
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx') 
sum(is.na(daily_means))

# select column for Windonshore installed
library(dplyr)
Windonshore_installed<- daily_means%>%select(date,`Wind onshore installed`)
head(Windonshore_installed,5)


#create data frame for yearly data points
year<-c(2015,2016,2017,2018,2019,2020,2021,2022)
Windonshore_product=c(37701,41168,47042,51633,52792,53184,54499,55289)
Windonshore_installed<-data.frame(year,Windonshore_product)
Windonshore_installed

#plot the Windoshore_installed series
plot(Windonshore_installed, type='o',main='Windonshore_installed yearly')
# Windonshore installed  by year

#Interpolation of windonshore_installed yearly for 365 days
df1<- data.frame(YEAR=(c(2015,2016,2017,2018,2019,2020,2021,2022)),
                 Windonshore_installed=c(37701,41168,47042,51633,52792,53184,54499,55289))

year_day <- function(x, years){
  f <- approxfun(years, x)
  n <- length(years)
  days <- unlist(lapply(years[-n], function(y) y + (0:364)/365))
  days <- c(days, years[n])
  list(x = days, y = f(days))
}
data<-as.data.frame(year_day(df1$Windonshore_installed,df1$YEAR))
head(data,5)
tail(data,5)
dim(data)

# exporting data to excel file to make same dinmension
library(xlsx)
write.xlsx(data,'/Users/latifaredoana/Desktop/Book.xlsx', sheetName = 'sheet4')

#Select first two columns for windonshore vaiables
daily_means_windonshore<-daily_means[,c(1,4)]
dim(daily_means_windonshore)
# declare data as time series data
Windonshore_ts<-ts(daily_means_windonshore$Windonshore, start=c(2015,1),end=c(2022,365),frequency=365)


#loading interpolated data with same dimension
data<- read_excel('interpol2.xlsx')
dim(data)

#Adding interpolated column with existing weekly series
newdata<-cbind(daily_means_windonshore, interpolate=data$y)
head(newdata)

class(newdata$Windonshore)
class(newdata$interpolate)

# Devide existing Solar series with inerpolated series
Windonshore_norm<-newdata$Windonshore / (newdata$interpolate)
summary(Windonshore_norm)
# install and loading packages
library(BBmisc)
# method = range for normalisation 
Windonshore_norm = normalize(Windonshore_norm, method = "range", range = c(0, 1))

# declare data as time series data
Windonshore_norm<-ts(Windonshore_norm, start=c(2015,1),end=c(2022,365),frequency=365)

#Visualization of normalized series
library(fpp2)
par(mfrow = c(1, 2))
plot(Windonshore_ts, ylab="daily Windonshore production", xlab="year", main=' Before normalization',col = 'blue4')
plot(Windonshore_norm, ylab="daily Windonshore Production", xlab="year", main='After normalization', col='aquamarine4')

adf.test(Windonshore_ts)
adf.test(Windonshore_norm)
#Time series plot for windoffshore power
library(fpp2)
autoplot(Windonshore)+ylab("Windonshore")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Windonshore production (Past 8 years)") +
  theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))


Windonshore_train <- window(Windonshore_norm, start=c(2015,1), end=c(2021,127))
Windon_test<-window(Windonshore_norm, start=c(2021,128),end=c(2022,365))


#For daily data we have frequency 365 and we can not model the series arima function since the R software does not support frequency more than 350.The Arima() and auto.arima() functions will allow a seasonal period up to m=350.
#An alternative approach is to use a dynamic harmonic regression model, the number of Fourier terms was selected by minimising the AICc. The order of the ARIMA model is also selected by minimising the AICc, although that is done within the auto.arima() function.

##Applying fourier dynamic regreesion model for frequency 365 with order of regressors, k=6-20 (the number of Fourier sin and cos pairs)
#For all the applied order the ARIMA(2,0,0) model remain same.

# # Set up harmonic regressors of order 6
harmonics <- fourier(Windonshore_train, K =6)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Windonshore_train, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Windonshore_train, K = 6, h = 2*365)
fcast1 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast1
autoplot(fcast1)+
  ylab("Windonshore production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order 8-20
harmonics <- fourier(Windonshore_train, K =10)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit2 <- auto.arima(Windonshore_train, xreg = harmonics, seasonal = FALSE)
fit2
# # Forecasts next 2 years
newharmonics <- fourier(Windonshore_train, K =10, h = 2*365)
fcast2 <- forecast(fit2, xreg = newharmonics)
# # Plot forecasts fcast2
autoplot(fcast2)+
  ylab("Windonshore power production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order 15
harmonics <- fourier(Windonshore_train, K =12)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit3 <- auto.arima(Windonshore_train, xreg = harmonics, seasonal = FALSE)
fit3
# # Forecasts next 2 years
newharmonics <- fourier(Windonshore_train, K = 15, h = 3*365)
fcast3 <- forecast(fit3, xreg = newharmonics)
# # Plot forecasts fcast3
autoplot(fcast3)+
  ylab("Windonshore production for the last 8 years") + xlab("Year")

# # Set up harmonic regressors of order 20
harmonics <- fourier(Windonshore_train, K =20)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit4 <- auto.arima(Windonshore_train, xreg = harmonics, seasonal = FALSE)
fit4
# # Forecasts next 2 years
newharmonics <- fourier(Windonshore_train, K = 20, h = 2*365)
fcast4 <- forecast(fit4, xreg = newharmonics)
# # Plot forecasts fcast4
autoplot(fcast4)+
  ylab("Windonshore production for the last 8 years") + xlab("Year")


accuracy(fcast1, Windonshore_test)
accuracy(fcast2,Windonshore_test)
accuracy(fcast3, Windonshore_test)
accuracy(fcast4, Windonshore_test)
accuracy(fcast5,Windonshore_test)


fcast3%>%autoplot()+autolayer(fitted(fit3), series=' Windon_train')+autolayer( Windon_test)+
  ylab(" daily Windonshore production") + xlab("Year")





