library(xts)
library(dplyr)
#loading the daily_means data file
library(readxl)#  package for loading excel file
daily_means<- read_excel('daily_means.xlsx') 
View(daily_means)
# select column for solar installed
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

year_week <- function(x, years){
  f <- approxfun(years, x)
  n <- length(years)
  weeks <- unlist(lapply(years[-n], function(y) y + (0:51)/52))
  weeks <- c(weeks, years[n])
  list(x = weeks, y = f(weeks))
}
data<-as.data.frame(year_week(df1$Solar_installed,df1$YEAR))
plot(data)
head(data,10)
tail(data,5)
dim(data)


#Select first two columns for Solar vaiables
daily_means_Solar<-daily_means[,c(1,2)]
#Converting  daily data to weekly using mean value for using frequency 52. 
daily_Solar_week<-apply.weekly(daily_means_Solar, mean)
dim(daily_Solar_week)

#loading interpolated data for same dimension
data<- read_excel('data.xlsx') 
dim(data)



#Adding interpolated column with existing weekly series
newdata<-cbind(daily_Solar_week, interpolate=data$y)
head(newdata)

class(newdata$Solar)
class(newdata$interpolate)

# Devide existing Solar series with inerpolated series
devide_solar<-newdata$Solar / as.numeric(newdata$interpolate)
summary(devide_solar)
plot(devide_solar, type='l')

# install and loading packages
library(BBmisc)
# method = range for normalisation 
Solar_norm = normalize(devide_solar, method = "range", range = c(0, 1))


# declare data as time series data
Solar_norm<-ts(Solar_norm, start=c(2015,1),end=c(2022,54),frequency=52)
Solar_ts<-ts(daily_Solar_week$Solar, start=c(2015,1),end=c(2022,54),frequency=52)
#Time series plot for Solar power
library(fpp2)
par(mfrow = c(1, 2))
plot(Solar_ts, ylab="Weekly Solar production", xlab="year", main=' Before normalization',col = 'cyan4')
plot(Solar_norm, ylab="Weekly Solar Production", xlab="year", main='After normalization', col='aquamarine4')

autoplot(Solar_ts)+ylab("Solar")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Solar power production before normalization") +
  theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))

autoplot(Solar)+ylab("Solar")+xlab("year")+geom_line() +
  theme_minimal() + 
  ggtitle("Solar power production after normalization") +
  theme(plot.title = element_text(hjust=0.5, size=16, face="bold"))

#Series appears use to investigate seasonality
ggseasonplot(Solar_norm)+ ggtitle("Seasonal plot: Solar power energy")
#power consumption from Solar is higher in summer season compare to winter season.
adf.test(Windonshore_ts)
adf.test(Solar_norm)
Solar_train <- window(Solar_norm, start=c(2015,1), end=c(2021,26))
Solar_test<-window(Solar_norm, start=c(2021,27), end=c(2022,52))

# # Set up harmonic regressors of order till 14
harmonics <- fourier(Solar_train, K =14)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit1 <- auto.arima(Solar_train, xreg = harmonics, seasonal = FALSE)
fit1
# # Forecasts next 2 years
newharmonics <- fourier(Solar_train, K = 14, h = 2*52)
fcast1 <- forecast(fit1, xreg = newharmonics)
# Check the residuals
checkresiduals(fit1)
# # Plot forecasts fcast1
autoplot(fcast1)+
  ylab("Solar power production for the last 8 years") + xlab("Year")

# # Set up harmonic regressors of order till 16
harmonics <- fourier(Solar_train, K =16)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit2 <- auto.arima(Solar_train, xreg = harmonics, seasonal = FALSE)
fit2
# Check the residuals
checkresiduals(fit1)
# # Forecasts next 2 years
newharmonics <- fourier(Solar_train, K = 16, h = 3*52)
fcast2 <- forecast(fit2, xreg = newharmonics)
# # Plot forecasts fcast2
autoplot(fcast2)+
  ylab("Solar power production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order till 18
harmonics <- fourier(Solar_train, K =18)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit3<- auto.arima(Solar_train, xreg = harmonics, seasonal = FALSE)
fit3


# # Forecasts next 2 years
newharmonics <- fourier(Solar_train, K = 18, h = 3*52)
fcast3 <- forecast(fit3, xreg = newharmonics)
# # Plot forecasts fcast3
autoplot(fcast3)+
  ylab("Solar power production for the last 8 years") + xlab("Year")


# # Set up harmonic regressors of order till 22
harmonics <- fourier(Solar_train, K =22)

# # Fit a dynamic regression model to fit. Set xreg equal to harmonics and seasonal to FALSE because seasonality is handled by the regressors.
fit4 <- auto.arima(Solar_train, xreg = harmonics, seasonal = FALSE)
fit4
# # Forecasts next 2 years
newharmonics <- fourier(Solar_train, K = 22, h = 2*52)
fcast4 <- forecast(fit4, xreg = newharmonics)
# # Plot forecasts fcast4
autoplot(fcast4)+
  ylab("Solar power production for the last 8 years") + xlab("Year")


accuracy(fcast1, Solar_test)
accuracy(fcast2, Solar_test)
accuracy(fcast3, Solar_test)
accuracy(fcast4, Solar_test)


fcast2%>%autoplot()+autolayer(fitted(fit2), series=' Solar_train')+autolayer( Solar_test)+
  ylab(" Weekly Solar production") + xlab("Year")

#Another approach using auto.arima function selecting model minimizing AICc value

bestfit <- list(aicc=Inf)
for(K in seq(25)) {
  fit <- auto.arima(Solar_train, xreg=fourier(Solar_train, K=K),
                    seasonal=FALSE)
  if(fit[["aicc"]] < bestfit[["aicc"]]) {
    bestfit <- fit
    bestK <- K
  }
}
fc <- forecast(bestfit,
             xreg=fourier(Solar_train, K=bestK, h=104))
autoplot(fc)
fc
K 


