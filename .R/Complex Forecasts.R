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

#################################  Linear Filters  ####################################################################### 

  ## we will use simple linear filters, such as moving average, to identify trends in our data
### Weekly Moving Average
WeeklyMAPrice <- filter(PriceData,filter=rep(1/5,5))
lines(WeeklyMAPrice,col="red") # plots the weekly stock movement 
### Monthly Moving Average
monthlyMAPrice <- filter(PriceData,filter=rep(1/25,25))  
lines(monthlyMAPrice,col="blue") # plots the monthly stock movement


# nicer graphs  
# If you don't want to save your plot, do not use png() and dev.off().
# png(file='gtemps1.png', width=600, height=320)
library(astsa)
tsplot(StockData$Adj.Close, gg=TRUE, ylab='Price', col=4, type='l')  # adjusted closing price
# dev.off() 

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

#################################  Forecasting  ####################################################################### 



###AR(p) (Autoregressive Model)
## With AR, future values are estimated using a weighted average of the past values.                            
  # We determine the order with ACF and PACF plots
    # In time series autocorrelation function measures

      # ACF plot portrays the correlation between series and it's lagged values. 
        acf(PriceData, lag.max = 10) 
          # Notice: we see that it does not tend to 0 between 5 to 6 lags, this means that there is no stationarity

        # PACF plot portrays the partial autocorrelation function by measuring the correlation of a time series with its own 
          # lagged values, controlling for the values of the time series at all shorter lags.
            pacf(PriceData, lag.max = 10)
              # Order: None, because we need stationarity. Our ACF portrays a non-stationary process.
              # More over, for an AR(p) model we need and ACF plot that decays slowly, 
              # and a PACF plot that cuts off after x lags; which would then lead us to conclude that
              # the order of AR(p=x).
                # We do not have any of those conditions, so we can't operate an AR(p) model
            
            
###MA(q) (Moving Average Model)
  # We consider the moving average of the past few forecast errors to forecast.
            # It needs to be stationary, and we know we do not have a stationary process.
            # More over, for an MA(q) model we need and ACF plot that decays sharply after x lags, 
            # and a PACF plot that decays slowly after x lags; which would then lead us to conclude that
            # the order of MA(q=x).
              # We do not have any of those conditions, so we can't operate an MA(q) model
            
          
                 

                            
###Arima
plot(PriceData) # we observe if the variance and/or mean vary through time
## Diagnostics # 1
  # Seasonality (in the plot, no seasonality was observed, but we may still do a test to confirm this)
    library(seastests)
      print("Testing the non-seasonal series")
        summary(wo(PriceData))
          # no seasonality detected
  # Trend Stationarity (just to confirm our observations from the plot, we will see the type of trend we have)
    trend.direction<-function(x){
      if(all(diff(x)>0)) return('Increasing')
      if(all(diff(x)<0)) return('Decreasing')
      return('Mixed')
      }
    trend.direction(PriceData)
    # Unit Root, 
      # Augmented Dickey Fuller Test
        library(tseries)
          adf.test(PriceData)
          # p-value> 0.05, we cannot reject the H0 of no-stationarity, and we conclude
          # that our variable is non-stationary
      # Kwiatkowski-Phillips-Schmidt-Shin 
          kpss.test(PriceData)
          # p-value< 0.05, we reject the H0 of no-stationarity, and we conclude
          # that our variable is stationary
          
 # Fixing for Stationarity         
          # I will apply a log first, to correct for the change in variance
          # I will apply a differentiation soon after, to correct for the change in mean
          # I will apply a drift/trend to correct for the trend 
              # By including a d=1 in our function, we aknowledge that there is a trend with slope ??
              # (often called "drift" in this context). In this case, the value of ?? is also an 
              # estimate of the mean of the differenced data. 
  PriceDiff <- diff(log(PriceData), differences=1)
  # let us identify if we still have stationarity
        kpss.test(PriceDiff)
  plot(PriceDiff)
    acf(PriceDiff, lag.max = 10) 
    pacf(PriceDiff, lag.max = 35) 
    # we see a sharp decline after lag 1
    # we see a slow decline after lag 5
    # this describes an MA(1)
    library(forecast)
      PriceArima <- Arima(PriceData, order=c(0,1,1), lambda=0, include.drift=TRUE)
        FutureForecast<-forecast(PriceArima,h=10)
          plot(FutureForecast)
            FutureForecast_F<-as.data.frame(FutureForecast)
              FutureForecast_F$`Point Forecast`
## Diagnostics #2
        Box.test(FutureForecast$residuals, lag=20, type="Ljung-Box")
        # A p-value < 0.05 rejects the null hypothesis that the 
        # time series isn't autocorrelated.
        # Conclusion: We have significant autocorrelation in the residuals
        # at lags 1-20
        
        # Fix serial autocorrelation? No.
          #  The main effect of serial correlation is that tests that assume 
          # independence will appear to have more 
          # statistical power than they really do. This does not "affect our data" but it might 
          # affect the confidence in statistical tests, artificially inflating their power. 
          # Given that our model's purpose is to simply measure patterns and apply 
          # forecasts based on these observations in time-series
          # data, we may proceed with caution.
        
        
        
        
        
### VAR  ######### INCLUIR VAR
######################## CORREGIR GARCH        
###GARCH
  ## When to use GARCH (generalized autoregressive conditional heteroscedasticity)
        # when the error variance is related to
        # squared error terms several periods in the past.
        # This is usually the case with  financial time series,
        # such as stock prices, inflation rates, and foreign exchange rates.
        #  This is because financial time series often exhibit the phenomenon of 
        # volatility clustering, that is, periods in which their prices show wide
        # swings for an extended time period followed by periods in which there is
        # relative calm. 
    # Why we are using GARCH
        # Therefore, in my opinion, the natural way to employ ARCH/GARCH is that after 
        # building an ARIMA model, if we found residuals with inconstant variance, we fit an 
        # ARCH/GARCH process to the variance of residuals 
        
        
  # Assumption # 1: Heteroschedasticty      
        library(TSA)
          h<-McLeod.Li.test(PriceArima)
            h$p.values
        # Given that we have p-values<0.05 we reject the Null Hypothesis
        # and conclude that we have an ARCH effect in our model. 
        # Hence, we have heteroschedasticity. 
        
  
    library(rugarch)
      snp<-StockData # <- read.zoo("DataChap4SP500.csv",header = TRUE, sep = ",",format="%m/%d/%Y")
        gspec.ru <- ugarchspec(mean.model=list( armaOrder=c(0,1)), distribution="std")
          gfit.ru <- ugarchfit(gspec.ru, snp$Adj.Close)
            coef(gfit.ru)
              FutureForecast=ugarchforecast(gfit.ru, n.ahead = 10)
                FutureForecast



###EGARCH
snp <- read.zoo("DataChap4SP500.csv",header = TRUE, sep = ",",format="%m/%d/%Y")
egarchsnp.spec = ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                            mean.model=list(armaOrder=c(0,0)))
egarchsnp.fit = ugarchfit(egarchsnp.spec, snp$Return)
egarchsnp.fit
coef(egarchsnp.fit)
FutureForecast=ugarchforecast(egarchsnp.fit, n.ahead = 5)
FutureForecast
###VGARCH
install.packages("rmgarch")
install.packages("PerformanceAnalytics")
library(rmgarch)
library(PerformanceAnalytics)
snpdji <- read.zoo("DataChap4SPDJIRet.csv",header = TRUE, sep = ",",format="%m/%d/%Y")

garch_spec = ugarchspec(mean.model = list(armaOrder = c(2,1)),variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")
dcc.garch_spec = dccspec(uspec = multispec( replicate(2, garch_spec) ), dccOrder = c(1,1), distribution = "mvnorm")
dcc_fit= dccfit(dcc.garch_spec,data = snpdji)
fcst=dccforecast(dcc_fit,n.ahead=5)
fcst
####DCC
snpdji <- read.zoo("DataChap4SPDJIRet.csv",header = TRUE, sep = ",",format="%m/%d/%Y")
garchspec = ugarchspec(mean.model = list(armaOrder = c(0,0)), 
                       variance.model = list(garchOrder = c(1,1), 
                                             model = "sGARCH"), distribution.model = "norm")

dcc.garchsnpdji.spec = dccspec(uspec = multispec( replicate(2, garchspec) ), dccOrder = c(1,1), distribution = "mvnorm")

dcc_fit = dccfit(dcc.garchsnpdji.spec , data = snpdji, fit.control=list(scale=TRUE))
dcc_fit
