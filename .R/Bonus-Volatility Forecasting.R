


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

plot(StockData$Adj.Close,type="l") # plots stock movement through time


#*log the difference of the variable to get the percentage change*


# log and difference the variable
GOOGLE_Price<-log(diff(StockData$Adj.Close))
library(zoo)
GOOGLE_Price <- na.locf(GOOGLE_Price) # to handle NA values (from weekends and holidays)
plot(GOOGLE_Price, ylab="",xlab="") 
abline(h=0, col="dark grey", lty=3, lwd=3)




# **Key Takeaway*: For the most part, GOOGLE stock performs at +2% -- +4% above average whenever the market has a positive shock. Similarly, it almost always performs above average, at about +1.5% -- +2%. Additionally, it has some very bad negative performance, at around -3% -- -4% when the market suffers a negative shock, but it immediately recuperates (by performing above average) the next day. 

#**Diagnostics**
acf(GOOGLE_Price, na.action=na.pass)

pacf(GOOGLE_Price, na.action=na.pass)


#**Note**: Our *ACF* portrays a non-stationary process. For an AR(p) model we need and ACF plot that decays slowly, and a PACF plot that cuts off after x lags; which would then lead us to conclude that the order of AR(p=x). We do not have any of those conditions, so we can't operate
# an AR(p) model.

# Our ACF decays slowly and ends at 3. Similarly, our PACF decays suddenly after the first spike, so it could be ARIMA(3,1,0) 


# ARIMA Model

ar3<-arima(GOOGLE_Price, c(3,1,0))
# ar3


# GARCH Model


library(rugarch)

GARCH_Model<-ugarchspec(mean.model = list(armaOrder = c(3,0)),variance.model = list(garchOrder = c(1,1), model = "sGARCH"), distribution.model = "norm")

GARCH_Model

GARCH_Fit<-ugarchfit(GARCH_Model, data=GOOGLE_Price)
GARCH_Fit



# __Notice__:
  
#  Based on the output, the estimated mean of the series is $\mu$=3.73786, and the estimated variance is $\omega+\alpha_1+\beta_1$=0.07028+0.07264+0.06565=0.20857


# Variance volatility series
# convert the fitted values into time series
library(readr)
StockData <- read_csv("GOOG.csv")
library(zoo)
dt = as.Date(StockData$Date, format="%Y-%m-%d")
Stockdataz = zoo(x=GARCH_Fit@fit$sigma^2, order.by=dt) 


# view the first values of our new time series
head(Stockdataz)



# view the last values of our new time series
tail(Stockdataz)


# **Visualize our model**

plot(Stockdataz, xlab="", ylab="", main=" GOOGLE Volatility - GARCH(1,1)")


# Forecast
# We will forecast the percentage change in GOOGLE stock for January

FutureForecast=ugarchforecast(GARCH_Fit, n.ahead = 20)
FutureForecast




'Stock Price'<-as.data.frame(FutureForecast@forecast)
`Stock Price`<-`Stock Price`$X2020.12.29.1
plot(`Stock Price`, col='red', type='l')
title("GOOGLE January Forecast")







# Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
