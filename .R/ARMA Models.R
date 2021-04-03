

# **Convert data into _time series_**


#################################  Convert data into time series  ####################################################################### 
setwd("C:/Users/ddaya/OneDrive/Data Science Portfolio/Quantitative Finance")
# import dataset

library(zoo)
StockData <- read.zoo("GOOG.csv",header = TRUE, sep = ",",format="%Y-%m-%d")
PriceData<-ts(StockData$Adj.Close, frequency = 5) # Frequency=5 because it is set on only business days

# Note:
#Frequency = 12 means that data is at monthly level
# Frequency = 4 means that data is at quarterly level
# Frequency = 6 means data points for every 10 minutes of an hour
# Frequency = 5 means that data is at daily level business day

plot(PriceData,type="l") # plots stock movement through time




## **Linear Filters**


# We will use simple linear filters, such as moving average, to identify trends in our data.




#################################  Linear Filters ####################################################################### 

plot(PriceData,type="l")
### Weekly Moving Average
WeeklyMAPrice <- filter(PriceData,filter=rep(1/5,5))
lines(WeeklyMAPrice,col="red") # plots the weekly stock movement 
### Monthly Moving Average
monthlyMAPrice <- filter(PriceData,filter=rep(1/25,25))  
lines(monthlyMAPrice,col="blue") # plots the monthly stock movement




# Nicer graph 1 (with _astsa_ package)

# nicer graphs  
# If you don't want to save your plot, do not use png() and dev.off().
# png(file='gtemps1.png', width=600, height=320)
library(astsa)
tsplot(StockData$Adj.Close, gg=TRUE, ylab='Price', col=4, type='l')  # adjusted closing price
# dev.off() 



# Nicer graph 2 (with _ggplot2_ package)




# png(file='gtemps1.png', width=600, height=320)
# multiple time series / comparative graph    
library(ggplot2)   
gtemp.df    = data.frame(Time=c(time(StockData$Adj.Close)), gtemp=c(WeeklyMAPrice), gtempk=c(monthlyMAPrice), gtempl=c(StockData$Adj.Close))
Price_plot<-ggplot(data = gtemp.df, aes(x=Time, y=value, color=variable )  )             +
  ylab('Price')                                 +
  geom_line(aes(y=gtemp , col='Stock Price'), size=1, alpha=.5)   +
  geom_line(aes(y=gtempl, col='Weekly MA'),  size=1, alpha=.5)   +
  geom_line(aes(y=gtempk, col='Monthly MA'),  size=1, alpha=.5)   +
  theme(legend.position=c(.1,.85))		
Price_plot
#  dev.off()




# **Forecasting**


###AR(p) (Autoregressive Model)  


# With AR, future values are estimated using a weighted average of the past values.                            
# We determine the order with ACF and PACF plots in time series autocorrelation function measurements.


##### ACF plot


#################################  Forecasting  ####################################################################### 



###AR(p) (Autoregressive Model)

# ACF plot portrays the correlation between series and it's lagged values. 
acf(PriceData, lag.max = 10) 




# __Notice__: we see that it does not tend to 0 between 5 to 6 lags, this means that there is no stationarity


##### PACF plot


# PACF plot portrays the partial autocorrelation function by measuring the correlation of a time series with its own lagged values, controlling for the values of the time series at all shorter lags.


# PACF plot
pacf(PriceData, lag.max = 10)


          


# __Order__: None, because we need stationarity.  


# __Note__: Our ACF portrays a non-stationary process. More over, for an AR(p) model we need and ACF plot that decays slowly, and a PACF plot that cuts off after x lags; which would then lead us to conclude that the order of AR(p=x). We do not have any of those conditions, so we can't operate an AR(p) model.


### MA(q) (Moving Average Model)  


# We consider the moving average of the past few forecast errors to forecast. It needs to be stationary, and we know we do not have a stationary process. More over, for an MA(q) model we need and ACF plot that decays sharply after x lags, and a PACF plot that decays slowly after x lags; which would then lead us to conclude that the order of MA(q=x). We do not have any of those conditions, so we can't operate an MA(q) model




### ARIMA (p,i,q) (Autoregressive Iterated Moving Average Model)

######Diagnostics 1:  _Seasonality_
# We observe if the variance and/or mean vary through time.

plot(PriceData) 



# In the plot, no seasonality was observed, but we should still do a test to confirm this.


library(seastests)
print("Testing the non-seasonal series")
summary(wo(PriceData))
        


# Results: No seasonality detected, but, if seasonality had been detected, we could have used _SARIMA_.


######Diagnostics 2: _Trend_


# Just to confirm our observations from the plot, we will see the type of trend we have.

trend.direction<-function(x){
  if(all(diff(x)>0)) return('Increasing')
  if(all(diff(x)<0)) return('Decreasing')
  return('Mixed')
}
trend.direction(PriceData)
 


###### Diagnostics 3: _Stationarity_ 


# Augmented Dickey Fuller Test (to detect Unit Root)

library(tseries)
adf.test(PriceData)

         


# __Conclusion__:  p-value> 0.05, we cannot reject the H0 of no-stationarity, and we conclude that our variable is non-stationary.


# Kwiatkowski-Phillips-Schmidt-Shin Test (to detect stationarity)
    
kpss.test(PriceData)



# __Conclusion__: P-value< 0.05, so we reject the H0 of stationarity, and we conclude that our variable is non-stationary.


###### Correcting for _Non-Stationarity_   


# I will apply a log first, to correct for the change in variance. 

# I will apply a differentiation soon after, to correct for the change in mean


# I will apply a drift/trend to correct for the trend 


# By including a d=1 in our function, we aknowledge that there is a trend with slope x (often called "drift" in this context). In this case, the value of x is also an estimate of the mean of the differenced data. 



PriceDiff <- diff(log(PriceData), differences=1)
head(PriceDiff)



# Let us identify if we still have stationarity.

kpss.test(PriceDiff)



# Visualize our variable of interest.

plot(PriceDiff)



# ACF plot:

acf(PriceDiff, lag.max = 10) 



# PACF plot:

pacf(PriceDiff, lag.max = 35) 



# In our ACF plot, see a sharp decline after lag 1.


# In our PACF plot, we see a slow decline after lag 5.


# This describes an MA(1).


# Let us forecast our ARIMA(0,1,1) model for the next 10 business days.

library(forecast)
PriceArima <- Arima(PriceData, order=c(0,1,1), lambda=0, include.drift=TRUE)
FutureForecast<-forecast(PriceArima,h=10)
plot(FutureForecast)
         


# Forecast 
          
FutureForecast_F<-as.data.frame(FutureForecast)
FutureForecast_F$`Point Forecast`
             






# _Model suitability_

Box.test(FutureForecast$residuals, lag=20, type="Ljung-Box")



# __Conclusion__: A p-value < 0.05 rejects the null hypothesis that the time series isn't autocorrelated. Therefore, we have significant autocorrelation in the residuals at lags 1-20
        
        
# How to fix autocorrelation in time series: After building an ARIMA model, we employ an ARCH/GARCH model if we found residuals with inconstant variance, we fit an ARCH/GARCH process to the variance of residuals 
        
        
# __Next Steps__: Should we fix serial autocorrelation? No.


# The main effect of serial correlation is that tests that assume independence will appear to have more statistical power than they really do. This does not "affect our data" but it might affect the confidence in statistical tests, artificially inflating their power. Given that our model's purpose is to simply measure patterns and apply forecasts based on these observations in time-series data, we may proceed with caution.

save.image("ARIMA_models.RData")

