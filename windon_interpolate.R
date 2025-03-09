library(xts)
#loading the daily_means data file
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx') 


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

year_week <- function(x, years){
  f <- approxfun(years, x)
  n <- length(years)
  weeks <- unlist(lapply(years[-n], function(y) y + (0:51)/52))
  weeks <- c(weeks, years[n])
  list(x = weeks, y = f(weeks))
}
data<-as.data.frame(year_week(df1$Windonshore_installed,df1$YEAR))
head(data,5)
tail(data,5)
dim(data)

# exporting data to excel file to make same dinmension
library(xlsx)
write.xlsx(data,'/Users/latifaredoana/Desktop/Book.xlsx', sheetName = 'sheet4')

#Select first two columns for windonshore vaiables
daily_means_windonshore<-daily_means[,c(1,4)]
dim(daily_means_windonshore)

#Converting  daily data to weekly using mean value for using frequency 52. 
daily_windonshore_week<-apply.weekly(daily_means_windonshore, mean)
dim(daily_windonshore_week)


#loading interpolated data with same dimension
data<- read_excel('data2.xlsx')
dim(data)


#Adding interpolated column with existing weekly series
newdata<-cbind(daily_windonshore_week, interpolate=data$y)
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
Windonshore_norm<-ts(Windonshore_norm, start=c(2015,1),end=c(2022,54),frequency=52)
Windonshore_ts<-ts(daily_windonshore_week$Windonshore, start=c(2015,1),end=c(2022,54),frequency=52)


#Time series plot for windoffshore power
par(mfrow = c(1, 2))
plot(Windonshore_ts, ylab="Windonshore", xlab="year", main='Weekly Production before normalization')
plot(Windonshore_norm, ylab="Windonshore", xlab="year", main='Weekly Production after normalization', col='blue')

#Time series plot for windoffshore power
library(fpp2)
autoplot(Windonshore)+ylab("Windonshore")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Windonshore production (Past 8 years)") +
  theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))
adf.test(Windonshore_norm)
# Create training and test set using 'window' function where using training data to estimate parameters 
# and the test data is used to evaluate its accuracy.
Windonshore_train <- window(Windonshore_norm, start=c(2015,1), end=c(2021,26))
Windonshore_test<-window(Windonshore_norm, start=c(2021,27), end=c(2022,52))

# # Set up harmonic regressors of order 6
harmonics <- fourier(Windonshore_train, K =6)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Windonshore_train, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Windonshore_train, K = 6, h = 2*52)
fcast1 <- forecast(fit1, xreg = newharmonics)
# # Plot forecasts fcast1
autoplot(fcast1)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order 8
harmonics <- fourier(Windoffshore_train, K =8)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit2 <- auto.arima(Windonshore_train, xreg = harmonics, seasonal = FALSE)
fit2
# # Forecasts next 2 years
newharmonics <- fourier(Windonshore_train, K =8, h = 2*52)
fcast2 <- forecast(fit2, xreg = newharmonics)
# # Plot forecasts fcast2
autoplot(fcast2)+
  ylab("Windoffshoreproduction for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order untill 10
harmonics <- fourier(Windonshore_train, K =10)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit3 <- auto.arima(Windonshore_train, xreg = harmonics, seasonal = FALSE)
fit3
# # Forecasts next 2 years
newharmonics <- fourier(Windonshore_train, K = 10, h = 2*52)
fcast3 <- forecast(fit3, xreg = newharmonics)
# # Plot forecasts fcast3
autoplot(fcast3)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order 11-15
harmonics <- fourier(Windonshore_train, K =12)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit4 <- auto.arima(Windonshore_train, xreg = harmonics, seasonal = FALSE)
fit4
# # Forecasts next 2 years
newharmonics <- fourier(Windonshore_train, K = 12, h = 3*52)
fcast4 <- forecast(fit4, xreg = newharmonics)
# # Plot forecasts fcast4
autoplot(fcast4)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")

# # Set up harmonic regressors of order 15
harmonics <- fourier(Windonshore_train, K =15)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit5 <- auto.arima(Windonshore_train, xreg = harmonics, seasonal = FALSE)
fit5
# # Forecasts next 2 years
newharmonics <- fourier(Windonshore_train, K = 15, h = 3*52)
fcast5 <- forecast(fit5, xreg = newharmonics)
# # Plot forecasts fcast5
autoplot(fcast4)+
  ylab("Windoffshore production for the last 8 years") + xlab("Year")

fcast4%>%autoplot()+autolayer(fitted(fit4), series=' Windonshore_train')+autolayer( Windonshore_test)+
  ylab(" Weekly windonshore production") + xlab("Year")

accuracy(fcast1, Windonshore_test)
accuracy(fcast2, Windonshore_test)
accuracy(fcast3, Windonshore_test)
accuracy(fcast4, Windonshore_test)
accuracy(fcast5, Windonshore_test)



# another approach using auto.arima function selecting model minimizing AICc value

bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(Windoffshore, xreg=fourier(Windoffshore, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}
fc <- forecast(bestfit,
               xreg=fourier(Windoffshore, K=bestK, h=104))
autoplot(fc)
