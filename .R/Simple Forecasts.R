setwd("C:/Users/ddaya/OneDrive/Quantitative Finance")

# import data
library(zoo)
StockData <- read.zoo("GOOG.csv",header = TRUE, sep = ",",format="%Y-%m-%d")
                              
#################################  Simple models  ####################################################################### 

  

## R Markdown

# This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.
 
  
  
  #Exploring the dataset
 
setwd("C:/Users/ddaya/OneDrive/Quantitative Finance")


# import data
library(zoo)
StockData <- read.zoo("GOOG.csv",header = TRUE, sep = ",",format="%Y-%m-%d")
PriceData<-ts(StockData$Adj.Close, frequency = 5) # Frequency=5 because it is set on only business days
summary(StockData)


head(StockData, nrow=10)





# Forecasting


#### Forecasting models 
####################################                
##################################################################################################################################                   
### Point Forecasts
# Note: A point forecast is the mean of all possible future sample paths. So the point forecasts are usually much less variable than the data.
# we will forecast for the next 10 days 

library(forecast)
m_ets = ets(PriceData) #exponential smoothing
f_ets = forecast(m_ets, h=10) # forecast de exponential smoothing
# plot( f_ets)

m_aa = auto.arima(PriceData)  # Auto ARIMA
f_aa = forecast(m_aa, h=10)   # forecast de ARIMA
# plot(f_aa)
m_ar<- arima(PriceData, order = c(5,2,0)) # ARIMA
f_ar<-forecast(m_ar, h=10)            
# plot(f_ar)
# m_tbats = tbats(PriceData) # Model for series exhibiting multiple complex seasonalities 
#  f_tbats = forecast(m_tbats, h=10) # Trigonometric regressors to model multiple-seasonalities
# plot(f_tbats)

# TBATS is an acronym for the following:
# T for trigonometric regressors to model multiple-seasonalities
# B for Box-Cox transformations
# A for ARMA errors
# T for trend
# S for seasonality
m_holt <- HoltWinters(PriceData, gamma=FALSE)
f_holt=forecast(m_holt, h = 10)   # forecast Holt's Exponential Smoothing
# plot(f_holt)
m_nai<- naive(PriceData) #naive bayes
f_nai<-forecast(m_nai, h=10) # forecast naive bayes
# plot(f_nai)
m_rwf<-rwf(PriceData) # random walk with drift model 
f_rwf<-forecast(m_rwf, h=10) # drift forecast
#   plot(f_rwf)
m_nn <- nnetar(PriceData)  # Neural Network
f_nn <- forecast(m_nn, h=10)       # forecast Neural Network
# plot(f_nn)
#  m_stlf<-stlm(PriceData)# Loess Forecasting model
#  f_stlf<-forecast(m_stlf, h=10) # Forecast Loess
#  plot(f_stlf)
#  library(fracdiff)
#   m_arf = arfima(PriceData)  # Auto ARIMA
#   f_arf = forecast(m_arf, h=10)   # forecast de ARIMA
#  plot(f_arf) # ARFIMA(p,d,q) model is selected and estimated automatically using 
# the Hyndman-Khandakar (2008) algorithm  to select 
# p and q and the Haslett and Raftery (1989) algorithm to estimate the parameters including d.
m_a <- ma(PriceData,order=5) # Moving Average
f_ma<-forecast(m_a, h=10) # forecast MA
# plot(f_ma)

m_ba<-baggedModel(PriceData, fn="auto.arima") # bagged ARIMA
f_ba<-forecast(m_ba, h=10)
# plot(f_ba)

### plot all forecasting models    
# png(file='gtemps1.png', width=600, height=320)  
par(mfrow=c(3,3)) # 3 columns and 3 rows of graphs
plot(f_ets)
plot(f_aa)
plot(f_ar)
# plot(f_tbats)
plot(f_holt)
plot(f_nai)
plot(f_rwf)
plot(f_nn)
# plot(f_stlf)
# plot(f_arf)
# plot(f_ma)
plot(f_ba)
#  dev.off() 



### Simulations of the most accurate forecasts

s_ets<-simulate(m_ets, nsim=10, future=TRUE, bootstrap=TRUE)
s_aa<-simulate(m_aa, nsim=10, future=TRUE, bootstrap=TRUE)
s_ar<-simulate(m_ar, nsim=10, future=TRUE, bootstrap=TRUE)
s_nn<-simulate(m_nn, nsim=10, future=TRUE, bootstrap=TRUE)
### Fit
si_ets<-simulate(m_ets, nsim=length(PriceData), bootstrap=TRUE)
si_aa<-simulate(m_aa, nsim=length(PriceData), bootstrap=TRUE)
si_ar<-simulate(m_ar, nsim=length(PriceData), bootstrap=TRUE)
si_nn<-simulate(m_nn, nsim=length(PriceData), bootstrap=TRUE)

### we generate graphs of the predicted values
library(ggplot2)   
gtemp.df    = data.frame(Time=c(time(s_ets)), gtemp=c(s_aa), gtempk=c(s_ets), gtempl=c(s_ar), gtempm=c(s_nn))
ggplot(data = gtemp.df, aes(x=Time, y=value, color=variable )  )             +
  ylab('Price')                                 +
  geom_line(aes(y=gtemp , col='ets'), size=1, alpha=.5)   +
  geom_line(aes(y=gtempk, col='auto.ARIMA'),  size=1, alpha=.5)   +
  geom_line(aes(y=gtempl, col='ARIMA'),  size=1, alpha=.5)   +
  geom_line(aes(y=gtempm, col='Neural Network'),  size=1, alpha=.5)   +
  theme(legend.position=c(.1,.85))	         


# Comparison between models




#################################  Comparison between models  ###########################################################

f1<-accuracy(m_ets) 
f2<-accuracy(m_aa)
f3<-accuracy(m_ar)
f4<-accuracy(m_nn)


Train<-rbind(f1,f2,f3,f4)
rownames(Train) <- c("m_ets","m_aa","m_ar","m_nn")

Train<-data.frame(Train)
Train[order(-Train$MAPE),]   # <---- select the model with smalles MAPE

                       

# Test
### We will test t=our two best model choices and select a final one

                         
dm.test(residuals(m_nn),residuals(m_ets), alternative = "less",
        h=10) # <--- Diebold-Mariano test compares the forecast accuracy of two forecast methods
# For alternative="less", the alternative hypothesis is that method 2 is less accurate than method 1. 




# According to the Diebold-Mariano Test, we conclude that ETS model is less accurate than the Neural Network model.

# Predict stock prices

                         


Forecasted_GOOG<-as.data.frame(f_nn)
names(Forecasted_GOOG)[1] <- "Stock"
dates<-seq(as.Date("2020/12/29"), by = 5, length.out = 10)
data.frame(dates,Forecasted_GOOG$Stock)








library(forecast)
 #### Forecasting models 
       #############################################################################################################################                       
      ################################### convert into time series in a different format  ##########################################                          
      # Create a daily Date object - helps my work on dates                                                                        #
         inds <- seq(as.Date("2019-12-31"), as.Date("2020-12-30"), by = "day") # Note: date variable is formatted as YYYY-MM-DD    #
                                                                                                                                   #
              ## Create a time series object                                                                                       #
                 Stock_Prices <- ts(StockData$Adj.Close,                                                                           #
                    start = c(2019, as.numeric(format(inds[1], "%j"))),                                                            #
                                                frequency = 365)         
                 
                 
  ##################################################################################################################################                   
  ### Point Forecasts
  # Note: A point forecast is the mean of all possible future sample paths. So the point forecasts are usually much less variable than the data.
     # we will forecast for the next 60 days                    
     m_ets = ets(Stock_Prices) #exponential smoothing
      f_ets = forecast(m_ets, h=10) # forecast de exponential smoothing
        plot(f_ets)
     m_aa = auto.arima(Stock_Prices)  # Auto ARIMA
      f_aa = forecast(m_aa, h=10)   # forecast de ARIMA
        plot(f_aa)
    m_ar<- arima(Stock_Prices, order = c(5,2,0)) # ARIMA
      f_ar<-forecast(m_ar, h=10)            
        plot(f_ar)
    m_tbats = tbats(Stock_Prices) # Model for series exhibiting multiple complex seasonalities 
      f_tbats = forecast(m_tbats, h=10) # Trigonometric regressors to model multiple-seasonalities
        plot(f_tbats)
        # TBATS is an acronym for the following:
          # T for trigonometric regressors to model multiple-seasonalities
          # B for Box-Cox transformations
          # A for ARMA errors
          # T for trend
          # S for seasonality
      m_holt <- HoltWinters(Stock_Prices, gamma=FALSE)
        f_holt=forecast(m_holt, h = 10)   # forecast Holt's Exponential Smoothing
          plot(f_holt)
  #  m_snaive <- snaive(Stock_Prices) #or snaive #Seasonal Naive
    #  f_snaive=forecast(m_snaive, h=10)  # forecast snaive 
      #  plot(f_snaive)
        
      #  m_rwf<-rwf(Stock_Prices) # random walk forecast
        #  f_rwf<-forecast(m_rwf, h=10)
         #   plot(f_rwf)
            
    m_nn <- nnetar(Stock_Prices)  # Neural Network
      f_nn <- forecast(m_nn, h=10)       # forecast Neural Network
        plot(f_nn)
    #m_stlf<-stlm(Stock_Prices)# Loess Forecasting model
     # f_stlf<-forecast(m_stlf, h=10) # Forecast Loess
      #  plot(f_stlf)
    library(fracdiff)
        m_arf = arfima(Stock_Prices)  # Auto ARIMA
          f_arf = forecast(m_arf, h=10)   # forecast de ARIMA
            plot(f_arf) # ARFIMA(p,d,q) model is selected and estimated automatically using 
                        # the Hyndman-Khandakar (2008) algorithm  to select 
                        # p and q and the Haslett and Raftery (1989) algorithm to estimate the parameters including d.
     m_a <- ma(Stock_Prices,order=5) # Moving Average
      f_ma<-forecast(a, h=10) # forecast MA
        plot(f_ma)
        
     
               
 ### plot all forecasting models    
        # png(file='gtemps1.png', width=600, height=320)  
        par(mfrow=c(3,3)) # 3 columns and 3 rows of graphs
        plot(f_ets)
        plot(f_aa)
        plot(f_ar)
        plot(f_tbats)
        plot(f_holt)
       # plot(f_snaive)
        plot(f_nn)
        #plot(f_stlf)
        plot(f_arf)
        plot(f_ma)
        #  dev.off() 
        
      
        
  ### Simulations of the most accurate forecasts
                         
      s_ets<-simulate(m_ets, nsim=60, future=TRUE, bootstrap=TRUE)
      s_aa<-simulate(m_aa, nsim=60, future=TRUE, bootstrap=TRUE)
      s_ar<-simulate(m_ar, nsim=60, future=TRUE, bootstrap=TRUE)
      s_nn<-simulate(m_nn, nsim=60, future=TRUE, bootstrap=TRUE)
          # Fit
            si_ets<-simulate(m_ets, nsim=length(Aqua_Prices), bootstrap=TRUE)
            si_aa<-simulate(m_aa, nsim=length(Aqua_Prices), bootstrap=TRUE)
            si_ar<-simulate(m_ar, nsim=length(Aqua_Prices), bootstrap=TRUE)
            si_nn<-simulate(m_nn, nsim=length(Aqua_Prices), bootstrap=TRUE)
                         
                         

                         #################################  Comparison between models  ###########################################################
                         library(forecast)
                         f1<-accuracy(m_ets) 
                         f2<-accuracy(m_aa)
                         f3<-accuracy(m_ar)
                         f4<-accuracy(m_tbats)
                         f5<-accuracy(f_holt)
                       #  f6<-accuracy(m_snaive)
                         f7<-accuracy(m_nn)
                       #  f8<-accuracy(m_stlf)
                         f9<-accuracy(m_arf)
                         f10<- accuracy(m_a)
                         
                         
                         Train<-rbind(f1,f2,f3,f4,f5,f6,f7,f8,f9)
                         rownames(Train) <- c("m_ets","m_aa","m_ar","m_tbats","f_holt","m_snaive","m_nn","m_stlf", "m_a")
                         
                         Train<-data.frame(Train)
                         Train[order(-Train$MAPE),]   # <---- select the model with smalles MAPE
                         
                         
                         
                         
                         dm.test(residuals(m_stlf),residuals(m_aa), alternative = "less",
                                 h=10) # <--- Diebold-Mariano test compares the forecast accuracy of two forecast methods
                         # For alternative="less", the alternative hypothesis is that method 2 is less accurate than method 1. 
                         
                         
                                 

