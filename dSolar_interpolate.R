library(tidyverse)
#loading the daily_means data file
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx') 

# select column for solar installed
library(dplyr)
Solar_installed<- daily_means%>%select(date,`Solar installed`)
head(Solar_installed,5)

#create data frame for yearly data points
year<-c(2015,2016,2017,2018,2019,2020,2021,2022)
Solar_product<-c(37271,38686,40834,42804,45299,48206,53302,57744)
Solar_installed<-data.frame(year,Solar_product)
Solar_installed

#plot the Solar_installed series
plot(Solar_installed, type='o',main='Solar_installed yearly')


#Interpolation of Solar_installed yearly for 52 weeks
df1<- data.frame(YEAR=(c(2015,2016,2017,2018,2019,2020,2021,2022 )),
                 Solar_installed= c(37271,38686,40834,42804,45299,48206,53302,57744))

year_day <- function(x, years){
  f <- approxfun(years, x)
  n <- length(years)
  days <- unlist(lapply(years[-n], function(y) y + (0:364)/365))
  days <- c(days, years[n])
  list(x = days, y = f(days))
}
data<-as.data.frame(year_day(df1$Solar_installed,df1$YEAR))
dim(data)

# exporting data to excel file to make same dinmension
library(xlsx)
write.xlsx(data,'/Users/latifaredoana/Desktop/Book.xlsx')

#Select first two columns for Solar vaiables
daily_Solar<-daily_means[,c(1,2)]
dim(daily_Solar)

#loading interpolated data for same dimension
data<- read_excel('interpol.xlsx') 
dim(data)

#Adding interpolated column with existing weekly series
newdata<-cbind(daily_Solar, interpolate=data$y)
head(newdata)
dim(newdata)

class(newdata$Solar)
class(newdata$interpolate)

# Devide existing Solar series with interpolated series
Solar_norm<-newdata$Solar / as.numeric(newdata$interpolate)
summary(Solar_norm)

#Visualization of normalized series
plot(Solar_norm, main='Visualization after normalization with interpolated values', 
     type='l', xlab= 'Solar_norm', ylab='year',col='black', col.axis = "blue")

# declare data as time series data
Solar<-ts(Solar_norm, start=c(2015,1),frequency=365)

#Time series plot for Solar power
library(fpp2)
autoplot(Solar)+ylab("Solar")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Solar power production (Past 8 years)") +
  theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))

#Series appears use to investigate seasonality
ggseasonplot(Solar)+ ggtitle("Seasonal plot: Solar power energy")
#power consumption from Solar is higher in summer season compare to winter season.

#For daily data we have frequency 365 and we can not model the series arima function since the R software does not support frequency more than 350.The Arima() and auto.arima() functions will allow a seasonal period up to m=350.
#An alternative approach is to use a dynamic harmonic regression model, the number of Fourier terms was selected by minimising the AICc. The order of the ARIMA model is also selected by minimising the AICc, although that is done within the auto.arima() function.

##Applying fourier dynamic regreesion model for frequency 365 with order of regressors, k=10-20 (the number of Fourier sin and cos pairs)


# # Set up harmonic regressors of order till 10
harmonics <- fourier(Solar, K =10)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Solar, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Solar, K = 10, h = 2*365)
fcast1 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast1
autoplot(fcast1)+
  ylab("Solar power production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order till 15
harmonics <- fourier(Solar, K =15)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Solar, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Solar, K = 15, h = 2*365)
fcast2 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast2
autoplot(fcast2)+
  ylab("Solar power production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order till 18
harmonics <- fourier(Solar, K =18)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Solar, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Solar, K = 18, h = 2*365)
fcast3 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast3
autoplot(fcast3)+
  ylab("Solar power production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order till 20
harmonics <- fourier(Solar, K =20)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Solar, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Solar, K = 20, h = 2*365)
fcast4 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast4
autoplot(fcast4)+
  ylab("Solar power production for the last 8 years") + xlab("Year")



